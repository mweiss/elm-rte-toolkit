module RichTextEditor.Editor exposing
    ( applyCommand
    , applyCommandNoForceSelection
    , applyNamedCommandList
    , update
    , updateEditorState
    , view
    )

import Array exposing (Array)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import RichTextEditor.Commands exposing (removeRangeSelection)
import RichTextEditor.Internal.BeforeInput as BeforeInput
import RichTextEditor.Internal.DomNode
    exposing
        ( decodeDomNode
        , extractRootEditorBlockNode
        , findTextChanges
        )
import RichTextEditor.Internal.Editor
import RichTextEditor.Internal.HtmlNode exposing (editorBlockNodeToHtmlNode)
import RichTextEditor.Internal.KeyDown as KeyDown
import RichTextEditor.Internal.Paste as Paste
import RichTextEditor.Model.Command exposing (NamedCommand, NamedCommandList, transformCommand)
import RichTextEditor.Model.Constants exposing (zeroWidthSpace)
import RichTextEditor.Model.Decorations exposing (Decorations, elementDecorators, markDecorators)
import RichTextEditor.Model.DomNode exposing (DomNode(..))
import RichTextEditor.Model.Editor
    exposing
        ( Editor
        , InternalEditorMsg(..)
        , Tagger
        , bufferedEditorState
        , completeRerenderCount
        , forceCompleteRerender
        , forceRerender
        , forceReselection
        , isComposing
        , renderCount
        , selectionCount
        , state
        , withBufferedEditorState
        , withComposing
        , withState
        )
import RichTextEditor.Model.Event exposing (EditorChange, PasteEvent, TextChange)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark as Mark exposing (Mark)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , Element
        , InlineLeaf(..)
        , InlineLeafTree(..)
        , Path
        , blockArray
        , childNodes
        , definitionFromElementParameters
        , elementParametersFromBlockNode
        , elementParametersFromInlineLeafParameters
        , fromBlockArray
        , fromInlineArray
        , inlineLeafArray
        , nameFromElementParameters
        , text
        , treeFromInlineArray
        , withChildNodes
        , withText
        )
import RichTextEditor.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , isCollapsed
        , rangeSelection
        )
import RichTextEditor.Model.Spec
    exposing
        ( Spec
        , toHtmlNodeFromMarkDefinition
        , toHtmlNodeFromNodeDefinition
        )
import RichTextEditor.Model.State as State exposing (State, withRoot, withSelection)
import RichTextEditor.Node exposing (Node(..), nodeAt)
import RichTextEditor.NodePath as NodePath exposing (toString)
import RichTextEditor.Selection exposing (annotateSelection, domToEditor, editorToDom)
import RichTextEditor.Spec exposing (childNodesPlaceholder)


updateSelection : Maybe Selection -> Bool -> Editor -> Editor
updateSelection maybeSelection isDomPath editor =
    let
        editorState =
            state editor
    in
    case maybeSelection of
        Nothing ->
            editor |> withState (editorState |> withSelection maybeSelection)

        Just selection ->
            let
                translatedSelection =
                    if isDomPath then
                        domToEditor (State.root editorState) selection

                    else
                        Just selection
            in
            editor |> withState (editorState |> withSelection translatedSelection)


update : InternalEditorMsg -> Editor -> Editor
update msg editor =
    case msg of
        ChangeEvent change ->
            updateChangeEvent change editor

        SelectionEvent selection isDomPath ->
            updateSelection selection isDomPath editor

        BeforeInputEvent inputEvent ->
            BeforeInput.handleBeforeInput inputEvent editor

        CompositionStart ->
            handleCompositionStart editor

        CompositionEnd ->
            handleCompositionEnd editor

        KeyDownEvent e ->
            KeyDown.handleKeyDown e editor

        PasteWithDataEvent e ->
            Paste.handlePaste e editor

        CutEvent ->
            handleCut editor


handleCut : Editor -> Editor
handleCut editor =
    case applyNamedCommandList [ ( "removeRangeSelection", transformCommand removeRangeSelection ) ] editor of
        Err _ ->
            editor

        Ok e ->
            forceRerender e


textChangesDomToEditor : BlockNode -> List TextChange -> Maybe (List TextChange)
textChangesDomToEditor editorNode changes =
    List.foldl
        (\( p, text ) maybeAgg ->
            case maybeAgg of
                Nothing ->
                    Nothing

                Just agg ->
                    case NodePath.domToEditor editorNode p of
                        Nothing ->
                            Nothing

                        Just translatedPath ->
                            Just (( translatedPath, text ) :: agg)
        )
        (Just [])
        changes


deriveTextChanges : BlockNode -> DomNode -> Result String (List TextChange)
deriveTextChanges editorNode domNode =
    let
        htmlNode =
            editorBlockNodeToHtmlNode editorNode
    in
    findTextChanges htmlNode domNode


applyForceFunctionOnEditor : (Editor -> Editor) -> Editor -> Editor
applyForceFunctionOnEditor rerenderFunc editor =
    rerenderFunc
        (case bufferedEditorState editor of
            Nothing ->
                editor

            Just bufferedEditorState ->
                let
                    newEditor =
                        updateEditorState "buffered" bufferedEditorState editor
                in
                newEditor
                    |> withBufferedEditorState Nothing
                    |> withComposing False
        )


updateChangeEvent : EditorChange -> Editor -> Editor
updateChangeEvent change editor =
    case change.characterDataMutations of
        Nothing ->
            case D.decodeValue decodeDomNode change.root of
                Err _ ->
                    editor

                Ok root ->
                    updateChangeEventFullScan root change.selection editor

        Just characterDataMutations ->
            updateChangeEventTextChanges (sanitizeMutations characterDataMutations) change.selection editor


sanitizeMutations : List TextChange -> List TextChange
sanitizeMutations changes =
    List.map
        (\( p, t ) ->
            if t == zeroWidthSpace then
                ( p, "" )

            else
                ( p, t )
        )
        changes


differentText : BlockNode -> TextChange -> Bool
differentText root ( path, t ) =
    case nodeAt path root of
        Nothing ->
            True

        -- We'll mark invalid paths as different since it will resolve later when we try to replace the node
        Just node ->
            case node of
                Inline il ->
                    case il of
                        TextLeaf tl ->
                            text tl /= t

                        _ ->
                            True

                -- Again, invalid paths will be resolved later, so just mark it as true
                _ ->
                    True


updateChangeEventTextChanges : List TextChange -> Maybe Selection -> Editor -> Editor
updateChangeEventTextChanges textChanges selection editor =
    case textChangesDomToEditor (State.root (state editor)) textChanges of
        Nothing ->
            applyForceFunctionOnEditor forceRerender editor

        Just changes ->
            let
                editorState =
                    state editor

                actualChanges =
                    List.filter (differentText (State.root editorState)) changes
            in
            if List.isEmpty actualChanges then
                editor

            else
                case replaceText (State.root editorState) actualChanges of
                    Nothing ->
                        applyForceFunctionOnEditor forceRerender editor

                    Just replacedEditorNodes ->
                        let
                            newEditorState =
                                editorState
                                    |> withSelection (selection |> Maybe.andThen (domToEditor (State.root editorState)))
                                    |> withRoot replacedEditorNodes
                        in
                        if isComposing editor then
                            editor
                                |> withBufferedEditorState (Just newEditorState)

                        else
                            let
                                newEditor =
                                    updateEditorState "textChange" newEditorState editor
                            in
                            applyForceFunctionOnEditor forceReselection newEditor


updateChangeEventFullScan : DomNode -> Maybe Selection -> Editor -> Editor
updateChangeEventFullScan domRoot selection editor =
    case extractRootEditorBlockNode domRoot of
        Nothing ->
            applyForceFunctionOnEditor forceCompleteRerender editor

        Just editorRootDomNode ->
            if needCompleteRerender domRoot then
                applyForceFunctionOnEditor forceCompleteRerender editor

            else
                case deriveTextChanges (State.root (state editor)) editorRootDomNode of
                    Ok changes ->
                        updateChangeEventTextChanges changes selection editor

                    Err _ ->
                        applyForceFunctionOnEditor forceRerender editor


needCompleteRerender : DomNode -> Bool
needCompleteRerender root =
    case root of
        DomNode v ->
            let
                cnodes =
                    Maybe.withDefault Array.empty v.childNodes
            in
            Array.length cnodes /= 1


editorChangeDecoder : D.Decoder InternalEditorMsg
editorChangeDecoder =
    D.map ChangeEvent
        (D.map3 EditorChange
            (D.at [ "detail", "root" ] D.value)
            (D.at [ "detail", "selection" ] selectionDecoder)
            (D.maybe (D.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder))
        )


characterDataMutationsDecoder : D.Decoder (List TextChange)
characterDataMutationsDecoder =
    D.list (D.map2 Tuple.pair (D.field "path" (D.list D.int)) (D.field "text" D.string))


onEditorChange : (InternalEditorMsg -> msg) -> Html.Attribute msg
onEditorChange msgFunc =
    Html.Events.on "editorchange" (D.map msgFunc editorChangeDecoder)


selectionDecoder : D.Decoder (Maybe Selection)
selectionDecoder =
    D.maybe
        (D.map4 rangeSelection
            (D.at [ "anchorNode" ] (D.list D.int))
            (D.at [ "anchorOffset" ] D.int)
            (D.at [ "focusNode" ] (D.list D.int))
            (D.at [ "focusOffset" ] D.int)
        )


editorSelectionChangeDecoder : D.Decoder InternalEditorMsg
editorSelectionChangeDecoder =
    D.map2 SelectionEvent
        (D.at [ "detail" ] selectionDecoder)
        (D.succeed True)


pasteWithDataDecoder : D.Decoder InternalEditorMsg
pasteWithDataDecoder =
    D.map PasteWithDataEvent <|
        D.map2
            PasteEvent
            (D.at [ "detail", "text" ] D.string)
            (D.at [ "detail", "html" ] D.string)


onCompositionStart : (InternalEditorMsg -> msg) -> Html.Attribute msg
onCompositionStart msgFunc =
    Html.Events.on "compositionstart" (D.map msgFunc (D.succeed CompositionStart))


onCompositionEnd : (InternalEditorMsg -> msg) -> Html.Attribute msg
onCompositionEnd msgFunc =
    Html.Events.on "compositionend" (D.map msgFunc (D.succeed CompositionEnd))


onPasteWithData : (InternalEditorMsg -> msg) -> Html.Attribute msg
onPasteWithData msgFunc =
    Html.Events.on "pastewithdata" (D.map msgFunc pasteWithDataDecoder)


onCut : (InternalEditorMsg -> msg) -> Html.Attribute msg
onCut msgFunc =
    Html.Events.on "cut" (D.map msgFunc (D.succeed CutEvent))


onEditorSelectionChange : (InternalEditorMsg -> msg) -> Html.Attribute msg
onEditorSelectionChange msgFunc =
    Html.Events.on "editorselectionchange" (D.map msgFunc editorSelectionChangeDecoder)


replaceText : BlockNode -> List TextChange -> Maybe BlockNode
replaceText editorNode changes =
    List.foldl
        (\change maybeNode ->
            case maybeNode of
                Nothing ->
                    Nothing

                Just node ->
                    applyTextChange node change
        )
        (Just editorNode)
        changes


applyTextChange : BlockNode -> TextChange -> Maybe BlockNode
applyTextChange editorNode ( path, text ) =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case childNodes editorNode of
                BlockChildren array ->
                    let
                        a =
                            fromBlockArray array
                    in
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just cblock ->
                            case applyTextChange cblock ( xs, text ) of
                                Nothing ->
                                    Nothing

                                Just textChangeNode ->
                                    Just <|
                                        (editorNode
                                            |> withChildNodes (blockArray <| Array.set x textChangeNode a)
                                        )

                InlineChildren array ->
                    let
                        a =
                            fromInlineArray array
                    in
                    if not <| List.isEmpty xs then
                        Nothing

                    else
                        case Array.get x a of
                            Nothing ->
                                Nothing

                            Just inlineNode ->
                                case inlineNode of
                                    TextLeaf contents ->
                                        Just
                                            (editorNode
                                                |> withChildNodes
                                                    (inlineLeafArray <|
                                                        Array.set x
                                                            (TextLeaf
                                                                (contents |> withText (String.replace zeroWidthSpace "" text))
                                                            )
                                                            a
                                                    )
                                            )

                                    _ ->
                                        Nothing

                Leaf ->
                    Nothing


selectionAttribute : Maybe Selection -> Int -> Int -> String
selectionAttribute maybeSelection renderCount selectionCount =
    case maybeSelection of
        Nothing ->
            "render-count=" ++ String.fromInt renderCount

        Just selection ->
            String.join ","
                [ "anchor-offset=" ++ String.fromInt (anchorOffset selection)
                , "anchor-node=" ++ toString (anchorNode selection)
                , "focus-offset=" ++ String.fromInt (focusOffset selection)
                , "focus-node=" ++ toString (focusNode selection)
                , "render-count=" ++ String.fromInt renderCount
                , "selection-count=" ++ String.fromInt selectionCount
                ]


onBeforeInput : Tagger msg -> Editor -> Html.Attribute msg
onBeforeInput tagger editor =
    Html.Events.preventDefaultOn "beforeinput" (BeforeInput.preventDefaultOnBeforeInputDecoder tagger editor)


onKeyDown : Tagger msg -> Editor -> Html.Attribute msg
onKeyDown tagger editor =
    Html.Events.preventDefaultOn "keydown" (KeyDown.preventDefaultOnKeyDownDecoder tagger editor)


handleCompositionStart : Editor -> Editor
handleCompositionStart editor =
    editor
        |> withComposing True


handleCompositionEnd : Editor -> Editor
handleCompositionEnd editor =
    case bufferedEditorState editor of
        Nothing ->
            editor |> withComposing False

        Just _ ->
            applyForceFunctionOnEditor forceReselection editor


shouldHideCaret : State -> Bool
shouldHideCaret editorState =
    case State.selection editorState of
        Nothing ->
            True

        Just selection ->
            if not <| isCollapsed selection then
                False

            else
                case nodeAt (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        False

                    Just node ->
                        case node of
                            Block _ ->
                                True

                            Inline leaf ->
                                case leaf of
                                    InlineLeaf _ ->
                                        True

                                    _ ->
                                        False


markCaretSelectionOnEditorNodes : State -> BlockNode
markCaretSelectionOnEditorNodes editorState =
    case State.selection editorState of
        Nothing ->
            State.root editorState

        Just selection ->
            if isCollapsed selection then
                annotateSelection selection (State.root editorState)

            else
                State.root editorState


editorToDomSelection : Editor -> Maybe Selection
editorToDomSelection editor =
    case State.selection (state editor) of
        Nothing ->
            Nothing

        Just selection ->
            editorToDom (State.root (state editor)) selection


view : Tagger msg -> Decorations msg -> Editor -> Html msg
view tagger decorations editor =
    let
        st =
            state editor
    in
    Html.Keyed.node "elm-editor"
        [ onEditorChange tagger
        , onEditorSelectionChange tagger
        , onCompositionStart tagger
        , onCompositionEnd tagger
        , onPasteWithData tagger
        , onCut tagger
        ]
        [ ( String.fromInt (completeRerenderCount editor)
          , Html.Keyed.node "div"
                [ Html.Attributes.contenteditable True
                , Html.Attributes.class "rte-main"
                , Html.Attributes.attribute "data-rte-main" "true"
                , Html.Attributes.classList [ ( "rte-hide-caret", shouldHideCaret st ) ]
                , onBeforeInput tagger editor
                , onKeyDown tagger editor
                ]
                [ ( String.fromInt (renderCount editor)
                  , viewEditorBlockNode
                        decorations
                        []
                        (markCaretSelectionOnEditorNodes st)
                  )
                ]
          )
        , ( "selectionstate"
          , Html.node "selection-state"
                [ Html.Attributes.attribute
                    "selection"
                    (selectionAttribute
                        (editorToDomSelection editor)
                        (renderCount editor)
                        (selectionCount editor)
                    )
                ]
                []
          )
        ]


renderHtmlNode : HtmlNode -> List (Path -> List (Html.Attribute msg)) -> Array (Html msg) -> Path -> Html msg
renderHtmlNode node decorators vdomChildren backwardsRelativePath =
    case node of
        ElementNode name attributes children ->
            let
                childNodes =
                    if children == childNodesPlaceholder then
                        vdomChildren

                    else
                        Array.indexedMap
                            (\i n -> renderHtmlNode n decorators vdomChildren (i :: backwardsRelativePath))
                            children
            in
            Html.node
                name
                (List.map (\( k, v ) -> Html.Attributes.attribute k v) attributes
                    ++ List.concatMap (\d -> d (List.reverse backwardsRelativePath)) decorators
                )
                (Array.toList childNodes)

        TextNode v ->
            Html.text v


viewMark : Decorations msg -> Path -> Mark -> Array (Html msg) -> Html msg
viewMark decorations backwardsNodePath mark children =
    let
        mDecorators =
            Maybe.withDefault []
                (Dict.get
                    (Mark.name mark)
                    (markDecorators decorations)
                )

        decorators =
            List.map (\d -> d (List.reverse backwardsNodePath) mark) mDecorators

        node =
            toHtmlNodeFromMarkDefinition (Mark.definition mark) mark childNodesPlaceholder
    in
    renderHtmlNode node decorators children []


viewElement : Decorations msg -> Element -> Path -> Array (Html msg) -> Html msg
viewElement decorations elementParameters backwardsNodePath children =
    let
        definition =
            definitionFromElementParameters elementParameters

        node =
            toHtmlNodeFromNodeDefinition definition elementParameters childNodesPlaceholder

        eDecorators =
            Maybe.withDefault []
                (Dict.get
                    (nameFromElementParameters elementParameters)
                    (elementDecorators decorations)
                )

        decorators =
            List.map (\d -> d (List.reverse backwardsNodePath) elementParameters) eDecorators

        nodeHtml =
            renderHtmlNode node decorators children []
    in
    nodeHtml


viewInlineLeafTree : Decorations msg -> Path -> Array InlineLeaf -> InlineLeafTree -> Html msg
viewInlineLeafTree decorations backwardsPath inlineLeafArray inlineLeafTree =
    case inlineLeafTree of
        LeafNode i ->
            case Array.get i inlineLeafArray of
                Just l ->
                    viewInlineLeaf decorations (i :: backwardsPath) l

                Nothing ->
                    -- Not the best thing, but what else can we do if we have an invalid tree?
                    -- This state should be impossible though.
                    Html.div [ Html.Attributes.class "rte-error" ] [ Html.text "Invalid leaf tree." ]

        MarkNode n ->
            viewMark decorations backwardsPath n.mark <|
                Array.map (viewInlineLeafTree decorations backwardsPath inlineLeafArray) n.children


viewEditorBlockNode : Decorations msg -> Path -> BlockNode -> Html msg
viewEditorBlockNode decorations backwardsPath node =
    viewElement decorations
        (elementParametersFromBlockNode node)
        backwardsPath
        (case childNodes node of
            BlockChildren l ->
                Array.indexedMap (\i n -> viewEditorBlockNode decorations (i :: backwardsPath) n) (fromBlockArray l)

            InlineChildren l ->
                Array.map (\n -> viewInlineLeafTree decorations backwardsPath (fromInlineArray l) n) (treeFromInlineArray l)

            Leaf ->
                Array.empty
        )


viewText : String -> Html msg
viewText text =
    Html.text
        (if String.isEmpty text then
            zeroWidthSpace

         else
            text
        )


viewInlineLeaf : Decorations msg -> Path -> InlineLeaf -> Html msg
viewInlineLeaf decorations backwardsPath leaf =
    case leaf of
        InlineLeaf l ->
            viewElement decorations (elementParametersFromInlineLeafParameters l) backwardsPath Array.empty

        TextLeaf v ->
            viewText (text v)


applyNamedCommandList : NamedCommandList -> Editor -> Result String Editor
applyNamedCommandList =
    RichTextEditor.Internal.Editor.applyNamedCommandList


applyCommand : NamedCommand -> Editor -> Result String Editor
applyCommand =
    RichTextEditor.Internal.Editor.applyCommand


applyCommandNoForceSelection : NamedCommand -> Editor -> Result String Editor
applyCommandNoForceSelection =
    RichTextEditor.Internal.Editor.applyCommandNoForceSelection


updateEditorState : String -> State -> Editor -> Editor
updateEditorState =
    RichTextEditor.Internal.Editor.updateEditorState
