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
import RichTextEditor.Internal.Constants exposing (zeroWidthSpace)
import RichTextEditor.Internal.DomNode
    exposing
        ( DomNode(..)
        , decodeDomNode
        , extractRootEditorBlockNode
        , findTextChanges
        )
import RichTextEditor.Internal.Editor
import RichTextEditor.Internal.HtmlNode exposing (childNodesPlaceholder, editorBlockNodeToHtmlNode)
import RichTextEditor.Internal.KeyDown as KeyDown
import RichTextEditor.Internal.Model.Editor
    exposing
        ( Editor
        , Message(..)
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
        , withShortKey
        , withState
        )
import RichTextEditor.Internal.Model.Event exposing (EditorChange, InitEvent, PasteEvent, TextChange)
import RichTextEditor.Internal.Paste as Paste
import RichTextEditor.Internal.Spec exposing (markDefinitionWithDefault, nodeDefinitionWithDefault)
import RichTextEditor.Model.Command exposing (CommandMap, NamedCommand, NamedCommandList, transform)
import RichTextEditor.Model.Decorations exposing (Decorations, elementDecorators, markDecorators)
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.InlineElement as InlineElement
import RichTextEditor.Model.Mark as Mark exposing (Mark)
import RichTextEditor.Model.MarkDefinition as MarkDefinition
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , InlineTree(..)
        , Path
        , blockChildren
        , childNodes
        , element
        , inlineChildren
        , toBlockArray
        , toInlineArray
        , toInlineTree
        , withChildNodes
        )
import RichTextEditor.Model.NodeDefinition as NodeDefinition
import RichTextEditor.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , isCollapsed
        , range
        )
import RichTextEditor.Model.Spec exposing (Spec)
import RichTextEditor.Model.State as State exposing (State, withRoot, withSelection)
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Node(..), nodeAt)
import RichTextEditor.Path as NodePath exposing (toString)
import RichTextEditor.Selection exposing (annotateSelection, domToEditor, editorToDom)


updateSelection : Maybe Selection -> Bool -> Spec -> Editor -> Editor
updateSelection maybeSelection isDomPath spec editor =
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
                        domToEditor spec (State.root editorState) selection

                    else
                        Just selection
            in
            editor |> withState (editorState |> withSelection translatedSelection)


update : CommandMap -> Spec -> Message -> Editor -> Editor
update commandMap spec msg editor =
    case msg of
        ChangeEvent change ->
            updateChangeEvent change spec editor

        SelectionEvent selection isDomPath ->
            updateSelection selection isDomPath spec editor

        BeforeInputEvent inputEvent ->
            BeforeInput.handleBeforeInput inputEvent commandMap spec editor

        CompositionStart ->
            handleCompositionStart editor

        CompositionEnd ->
            handleCompositionEnd editor

        KeyDownEvent e ->
            KeyDown.handleKeyDown e commandMap spec editor

        PasteWithDataEvent e ->
            Paste.handlePaste e spec editor

        CutEvent ->
            handleCut spec editor

        Init e ->
            handleInitEvent e editor

        ReplaceWith e ->
            e


handleInitEvent : InitEvent -> Editor -> Editor
handleInitEvent initEvent editor =
    editor |> withShortKey initEvent.shortKey


handleCut : Spec -> Editor -> Editor
handleCut spec editor =
    case applyNamedCommandList [ ( "removeRangeSelection", transform removeRangeSelection ) ] spec editor of
        Err _ ->
            editor

        Ok e ->
            forceRerender e


textChangesDomToEditor : Spec -> Block -> List TextChange -> Maybe (List TextChange)
textChangesDomToEditor spec editorNode changes =
    List.foldl
        (\( p, text ) maybeAgg ->
            case maybeAgg of
                Nothing ->
                    Nothing

                Just agg ->
                    case NodePath.domToEditor spec editorNode p of
                        Nothing ->
                            Nothing

                        Just translatedPath ->
                            Just (( translatedPath, text ) :: agg)
        )
        (Just [])
        changes


deriveTextChanges : Spec -> Block -> DomNode -> Result String (List TextChange)
deriveTextChanges spec editorNode domNode =
    let
        htmlNode =
            editorBlockNodeToHtmlNode spec editorNode
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


updateChangeEvent : EditorChange -> Spec -> Editor -> Editor
updateChangeEvent change spec editor =
    case change.characterDataMutations of
        Nothing ->
            case D.decodeValue decodeDomNode change.root of
                Err _ ->
                    editor

                Ok root ->
                    updateChangeEventFullScan root change.selection spec editor

        Just characterDataMutations ->
            updateChangeEventTextChanges (sanitizeMutations characterDataMutations) change.selection spec editor


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


differentText : Block -> TextChange -> Bool
differentText root ( path, t ) =
    case nodeAt path root of
        Nothing ->
            True

        -- We'll mark invalid paths as different since it will resolve later when we try to replace the node
        Just node ->
            case node of
                Inline il ->
                    case il of
                        Text tl ->
                            Text.text tl /= t

                        _ ->
                            True

                -- Again, invalid paths will be resolved later, so just mark it as true
                _ ->
                    True


updateChangeEventTextChanges : List TextChange -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventTextChanges textChanges selection spec editor =
    case textChangesDomToEditor spec (State.root (state editor)) textChanges of
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
                                    |> withSelection (selection |> Maybe.andThen (domToEditor spec (State.root editorState)))
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


updateChangeEventFullScan : DomNode -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventFullScan domRoot selection spec editor =
    case extractRootEditorBlockNode domRoot of
        Nothing ->
            applyForceFunctionOnEditor forceCompleteRerender editor

        Just editorRootDomNode ->
            if needCompleteRerender domRoot then
                applyForceFunctionOnEditor forceCompleteRerender editor

            else
                case deriveTextChanges spec (State.root (state editor)) editorRootDomNode of
                    Ok changes ->
                        updateChangeEventTextChanges changes selection spec editor

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


editorChangeDecoder : D.Decoder Message
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


onEditorChange : (Message -> msg) -> Html.Attribute msg
onEditorChange msgFunc =
    Html.Events.on "editorchange" (D.map msgFunc editorChangeDecoder)


selectionDecoder : D.Decoder (Maybe Selection)
selectionDecoder =
    D.maybe
        (D.map4 range
            (D.at [ "anchorNode" ] (D.list D.int))
            (D.at [ "anchorOffset" ] D.int)
            (D.at [ "focusNode" ] (D.list D.int))
            (D.at [ "focusOffset" ] D.int)
        )


editorSelectionChangeDecoder : D.Decoder Message
editorSelectionChangeDecoder =
    D.map2 SelectionEvent
        (D.at [ "detail" ] selectionDecoder)
        (D.succeed True)


pasteWithDataDecoder : D.Decoder Message
pasteWithDataDecoder =
    D.map PasteWithDataEvent <|
        D.map2
            PasteEvent
            (D.at [ "detail", "text" ] D.string)
            (D.at [ "detail", "html" ] D.string)


initDecoder : D.Decoder Message
initDecoder =
    D.map Init <|
        D.map
            InitEvent
            (D.at [ "detail", "shortKey" ] D.string)


onCompositionStart : (Message -> msg) -> Html.Attribute msg
onCompositionStart msgFunc =
    Html.Events.on "compositionstart" (D.map msgFunc (D.succeed CompositionStart))


onCompositionEnd : (Message -> msg) -> Html.Attribute msg
onCompositionEnd msgFunc =
    Html.Events.on "compositionend" (D.map msgFunc (D.succeed CompositionEnd))


onPasteWithData : (Message -> msg) -> Html.Attribute msg
onPasteWithData msgFunc =
    Html.Events.on "pastewithdata" (D.map msgFunc pasteWithDataDecoder)


onCut : (Message -> msg) -> Html.Attribute msg
onCut msgFunc =
    Html.Events.on "cut" (D.map msgFunc (D.succeed CutEvent))


onInit : (Message -> msg) -> Html.Attribute msg
onInit msgFunc =
    Html.Events.on "editorinit" (D.map msgFunc initDecoder)


onEditorSelectionChange : (Message -> msg) -> Html.Attribute msg
onEditorSelectionChange msgFunc =
    Html.Events.on "editorselectionchange" (D.map msgFunc editorSelectionChangeDecoder)


replaceText : Block -> List TextChange -> Maybe Block
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


applyTextChange : Block -> TextChange -> Maybe Block
applyTextChange editorNode ( path, text ) =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case childNodes editorNode of
                BlockChildren array ->
                    let
                        a =
                            toBlockArray array
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
                                            |> withChildNodes (blockChildren <| Array.set x textChangeNode a)
                                        )

                InlineChildren array ->
                    let
                        a =
                            toInlineArray array
                    in
                    if not <| List.isEmpty xs then
                        Nothing

                    else
                        case Array.get x a of
                            Nothing ->
                                Nothing

                            Just inlineNode ->
                                case inlineNode of
                                    Text contents ->
                                        Just
                                            (editorNode
                                                |> withChildNodes
                                                    (inlineChildren <|
                                                        Array.set x
                                                            (Text
                                                                (contents |> Text.withText (String.replace zeroWidthSpace "" text))
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


onBeforeInput : Tagger msg -> CommandMap -> Spec -> Editor -> Html.Attribute msg
onBeforeInput tagger commandMap spec editor =
    Html.Events.preventDefaultOn "beforeinput" (BeforeInput.preventDefaultOnBeforeInputDecoder tagger commandMap spec editor)


onKeyDown : Tagger msg -> CommandMap -> Spec -> Editor -> Html.Attribute msg
onKeyDown tagger commandMap spec editor =
    Html.Events.preventDefaultOn "keydown" (KeyDown.preventDefaultOnKeyDownDecoder tagger commandMap spec editor)


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
                                    InlineElement _ ->
                                        True

                                    _ ->
                                        False


markCaretSelectionOnEditorNodes : State -> Block
markCaretSelectionOnEditorNodes editorState =
    case State.selection editorState of
        Nothing ->
            State.root editorState

        Just selection ->
            if isCollapsed selection then
                annotateSelection selection (State.root editorState)

            else
                State.root editorState


editorToDomSelection : Spec -> Editor -> Maybe Selection
editorToDomSelection spec editor =
    case State.selection (state editor) of
        Nothing ->
            Nothing

        Just selection ->
            editorToDom spec (State.root (state editor)) selection


view : Tagger msg -> Decorations msg -> CommandMap -> Spec -> Editor -> Html msg
view tagger decorations commandMap spec editor =
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
        , onInit tagger
        ]
        [ ( String.fromInt (completeRerenderCount editor)
          , Html.Keyed.node "div"
                [ Html.Attributes.contenteditable True
                , Html.Attributes.class "rte-main"
                , Html.Attributes.attribute "data-rte-main" "true"
                , Html.Attributes.classList [ ( "rte-hide-caret", shouldHideCaret st ) ]
                , onBeforeInput tagger commandMap spec editor
                , onKeyDown tagger commandMap spec editor
                ]
                [ ( String.fromInt (renderCount editor)
                  , viewEditorBlockNode
                        spec
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
                        (editorToDomSelection spec editor)
                        (renderCount editor)
                        (selectionCount editor)
                    )
                ]
                []
          )
        ]


viewHtmlNode : HtmlNode -> List (Path -> List (Html.Attribute msg)) -> Array (Html msg) -> Path -> Html msg
viewHtmlNode node decorators vdomChildren backwardsRelativePath =
    case node of
        ElementNode name attributes children ->
            let
                childNodes =
                    if children == childNodesPlaceholder then
                        vdomChildren

                    else
                        Array.indexedMap
                            (\i n -> viewHtmlNode n decorators vdomChildren (i :: backwardsRelativePath))
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


viewMark : Spec -> Decorations msg -> Path -> Mark -> Array (Html msg) -> Html msg
viewMark spec decorations backwardsNodePath mark children =
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
            MarkDefinition.toHtmlNode (markDefinitionWithDefault mark spec) mark childNodesPlaceholder
    in
    viewHtmlNode node decorators children []


viewElement : Spec -> Decorations msg -> Element -> Path -> Array (Html msg) -> Html msg
viewElement spec decorations elementParameters backwardsNodePath children =
    let
        definition =
            nodeDefinitionWithDefault elementParameters spec

        node =
            NodeDefinition.toHtmlNode definition elementParameters childNodesPlaceholder

        eDecorators =
            Maybe.withDefault []
                (Dict.get
                    (Element.name elementParameters)
                    (elementDecorators decorations)
                )

        decorators =
            List.map (\d -> d (List.reverse backwardsNodePath) elementParameters) eDecorators

        nodeHtml =
            viewHtmlNode node decorators children []
    in
    nodeHtml


viewInlineLeafTree : Spec -> Decorations msg -> Path -> Array Inline -> InlineTree -> Html msg
viewInlineLeafTree spec decorations backwardsPath inlineLeafArray inlineLeafTree =
    case inlineLeafTree of
        LeafNode i ->
            case Array.get i inlineLeafArray of
                Just l ->
                    viewInlineLeaf spec decorations (i :: backwardsPath) l

                Nothing ->
                    -- Not the best thing, but what else can we do if we have an invalid tree?
                    -- This state should be impossible though.
                    Html.div [ Html.Attributes.class "rte-error" ] [ Html.text "Invalid leaf tree." ]

        MarkNode n ->
            viewMark spec decorations backwardsPath n.mark <|
                Array.map (viewInlineLeafTree spec decorations backwardsPath inlineLeafArray) n.children


viewEditorBlockNode : Spec -> Decorations msg -> Path -> Block -> Html msg
viewEditorBlockNode spec decorations backwardsPath node =
    viewElement spec
        decorations
        (element node)
        backwardsPath
        (case childNodes node of
            BlockChildren l ->
                Array.indexedMap (\i n -> viewEditorBlockNode spec decorations (i :: backwardsPath) n) (toBlockArray l)

            InlineChildren l ->
                Array.map (\n -> viewInlineLeafTree spec decorations backwardsPath (toInlineArray l) n) (toInlineTree l)

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


viewInlineLeaf : Spec -> Decorations msg -> Path -> Inline -> Html msg
viewInlineLeaf spec decorations backwardsPath leaf =
    case leaf of
        InlineElement l ->
            viewElement spec decorations (InlineElement.element l) backwardsPath Array.empty

        Text v ->
            viewText (Text.text v)


applyNamedCommandList : NamedCommandList -> Spec -> Editor -> Result String Editor
applyNamedCommandList =
    RichTextEditor.Internal.Editor.applyNamedCommandList


applyCommand : NamedCommand -> Spec -> Editor -> Result String Editor
applyCommand =
    RichTextEditor.Internal.Editor.applyCommand


applyCommandNoForceSelection : NamedCommand -> Spec -> Editor -> Result String Editor
applyCommandNoForceSelection =
    RichTextEditor.Internal.Editor.applyCommandNoForceSelection


updateEditorState : String -> State -> Editor -> Editor
updateEditorState =
    RichTextEditor.Internal.Editor.updateEditorState
