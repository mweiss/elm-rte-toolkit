module RichText.Editor exposing
    ( Editor
    , Message
    , apply
    , applyList
    , applyNoForceSelection
    , history
    , init
    , shortKey
    , state
    , update
    , view
    , withHistory
    )

import Array exposing (Array)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import RichText.Annotation exposing (annotateSelection)
import RichText.Commands exposing (removeRangeSelection)
import RichText.Config.Command exposing (CommandMap, NamedCommand, NamedCommandList, transform)
import RichText.Config.Decorations exposing (Decorations, elementDecorators, markDecorators)
import RichText.Config.MarkDefinition as MarkDefinition
import RichText.Config.NodeDefinition as NodeDefinition
import RichText.Config.Spec exposing (Spec)
import RichText.Internal.BeforeInput as BeforeInput
import RichText.Internal.Constants exposing (zeroWidthSpace)
import RichText.Internal.DomNode
    exposing
        ( DomNode(..)
        , decodeDomNode
        , extractRootEditorBlockNode
        , findTextChanges
        )
import RichText.Internal.HtmlNode exposing (childNodesPlaceholder, editorBlockNodeToHtmlNode)
import RichText.Internal.KeyDown as KeyDown
import RichText.Internal.Model.Editor as InternalEditor
    exposing
        ( Editor
        , Message(..)
        , Tagger
        , applyNamedCommandList
        , bufferedEditorState
        , completeRerenderCount
        , forceCompleteRerender
        , forceRerender
        , forceReselection
        , isComposing
        , renderCount
        , selectionCount
        , state
        , updateEditorStateWithTimestamp
        , withBufferedEditorState
        , withComposing
        , withShortKey
        , withState
        )
import RichText.Internal.Model.Event exposing (EditorChange, InitEvent, PasteEvent, TextChange)
import RichText.Internal.Paste as Paste
import RichText.Internal.Path as NodePath
import RichText.Internal.Selection exposing (domToEditor, editorToDom)
import RichText.Internal.Spec exposing (markDefinitionWithDefault, nodeDefinitionWithDefault)
import RichText.Model.Element as Element exposing (Element)
import RichText.Model.History exposing (History)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark as Mark exposing (Mark)
import RichText.Model.Node
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
        , toString
        , withChildNodes
        )
import RichText.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , isCollapsed
        , range
        )
import RichText.Model.State as State exposing (State, withRoot, withSelection)
import RichText.Model.Text as Text
import RichText.Node exposing (Node(..), nodeAt)


updateSelection : Maybe Selection -> Bool -> Spec -> Editor -> Editor
updateSelection maybeSelection isDomPath spec editor_ =
    let
        editorState =
            state editor_
    in
    case maybeSelection of
        Nothing ->
            editor_ |> withState (editorState |> withSelection maybeSelection)

        Just selection ->
            let
                translatedSelection =
                    if isDomPath then
                        domToEditor spec (State.root editorState) selection

                    else
                        Just selection
            in
            editor_ |> withState (editorState |> withSelection translatedSelection)


update : CommandMap -> Spec -> Message -> Editor -> Editor
update commandMap spec msg editor_ =
    case msg of
        ChangeEvent change ->
            updateChangeEvent change spec editor_

        SelectionEvent selection isDomPath ->
            updateSelection selection isDomPath spec editor_

        BeforeInputEvent inputEvent ->
            BeforeInput.handleBeforeInput inputEvent commandMap spec editor_

        CompositionStart ->
            handleCompositionStart editor_

        CompositionEnd ->
            handleCompositionEnd editor_

        KeyDownEvent e ->
            KeyDown.handleKeyDown e commandMap spec editor_

        PasteWithDataEvent e ->
            Paste.handlePaste e spec editor_

        CutEvent ->
            handleCut spec editor_

        Init e ->
            handleInitEvent e editor_

        ReplaceWith e ->
            e


handleInitEvent : InitEvent -> Editor -> Editor
handleInitEvent initEvent editor_ =
    editor_ |> withShortKey initEvent.shortKey


handleCut : Spec -> Editor -> Editor
handleCut spec editor_ =
    case applyList [ ( "removeRangeSelection", transform removeRangeSelection ) ] spec editor_ of
        Err _ ->
            editor_

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
applyForceFunctionOnEditor rerenderFunc editor_ =
    rerenderFunc
        (case bufferedEditorState editor_ of
            Nothing ->
                editor_

            Just bufferedEditorState ->
                let
                    newEditor =
                        InternalEditor.updateEditorState "buffered" bufferedEditorState editor_
                in
                newEditor
                    |> withBufferedEditorState Nothing
                    |> withComposing False
        )


updateChangeEvent : EditorChange -> Spec -> Editor -> Editor
updateChangeEvent change spec editor_ =
    case change.characterDataMutations of
        Nothing ->
            case D.decodeValue decodeDomNode change.root of
                Err _ ->
                    editor_

                Ok root ->
                    updateChangeEventFullScan change.timestamp root change.selection spec editor_

        Just characterDataMutations ->
            updateChangeEventTextChanges
                change.timestamp
                (sanitizeMutations characterDataMutations)
                change.selection
                spec
                editor_


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


updateChangeEventTextChanges : Int -> List TextChange -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventTextChanges timestamp textChanges selection spec editor_ =
    case textChangesDomToEditor spec (State.root (state editor_)) textChanges of
        Nothing ->
            applyForceFunctionOnEditor forceRerender editor_

        Just changes ->
            let
                editorState =
                    state editor_

                actualChanges =
                    List.filter (differentText (State.root editorState)) changes
            in
            if List.isEmpty actualChanges then
                editor_

            else
                case replaceText (State.root editorState) actualChanges of
                    Nothing ->
                        applyForceFunctionOnEditor forceRerender editor_

                    Just replacedEditorNodes ->
                        let
                            newEditorState =
                                editorState
                                    |> withSelection (selection |> Maybe.andThen (domToEditor spec (State.root editorState)))
                                    |> withRoot replacedEditorNodes
                        in
                        if isComposing editor_ then
                            editor_
                                |> withBufferedEditorState (Just newEditorState)

                        else
                            let
                                newEditor =
                                    updateEditorStateWithTimestamp (Just timestamp) "textChange" newEditorState editor_
                            in
                            applyForceFunctionOnEditor forceReselection newEditor


updateChangeEventFullScan : Int -> DomNode -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventFullScan timestamp domRoot selection spec editor_ =
    case extractRootEditorBlockNode domRoot of
        Nothing ->
            applyForceFunctionOnEditor forceCompleteRerender editor_

        Just editorRootDomNode ->
            if needCompleteRerender domRoot then
                applyForceFunctionOnEditor forceCompleteRerender editor_

            else
                case deriveTextChanges spec (State.root (state editor_)) editorRootDomNode of
                    Ok changes ->
                        updateChangeEventTextChanges timestamp changes selection spec editor_

                    Err _ ->
                        applyForceFunctionOnEditor forceRerender editor_


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
        (D.map4 EditorChange
            (D.at [ "detail", "root" ] D.value)
            (D.at [ "detail", "selection" ] selectionDecoder)
            (D.maybe (D.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder))
            (D.at [ "detail", "timestamp" ] D.int)
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
onBeforeInput tagger commandMap spec editor_ =
    Html.Events.preventDefaultOn "beforeinput" (BeforeInput.preventDefaultOnBeforeInputDecoder tagger commandMap spec editor_)


onKeyDown : Tagger msg -> CommandMap -> Spec -> Editor -> Html.Attribute msg
onKeyDown tagger commandMap spec editor_ =
    Html.Events.preventDefaultOn "keydown" (KeyDown.preventDefaultOnKeyDownDecoder tagger commandMap spec editor_)


handleCompositionStart : Editor -> Editor
handleCompositionStart editor_ =
    editor_
        |> withComposing True


handleCompositionEnd : Editor -> Editor
handleCompositionEnd editor_ =
    case bufferedEditorState editor_ of
        Nothing ->
            editor_ |> withComposing False

        Just _ ->
            applyForceFunctionOnEditor forceReselection editor_


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
editorToDomSelection spec editor_ =
    case State.selection (state editor_) of
        Nothing ->
            Nothing

        Just selection ->
            editorToDom spec (State.root (state editor_)) selection


view : Tagger msg -> Decorations msg -> CommandMap -> Spec -> Editor -> Html msg
view tagger decorations commandMap spec editor_ =
    let
        st =
            state editor_
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
        [ ( String.fromInt (completeRerenderCount editor_)
          , Html.Keyed.node "div"
                [ Html.Attributes.contenteditable True
                , Html.Attributes.class "rte-main"
                , Html.Attributes.attribute "data-rte-main" "true"
                , Html.Attributes.classList [ ( "rte-hide-caret", shouldHideCaret st ) ]
                , onBeforeInput tagger commandMap spec editor_
                , onKeyDown tagger commandMap spec editor_
                ]
                [ ( String.fromInt (renderCount editor_)
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
                        (editorToDomSelection spec editor_)
                        (renderCount editor_)
                        (selectionCount editor_)
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


{-| `Editor` represents the entire state of the editor, and is what you store in your model.
-}
type alias Editor =
    InternalEditor.Editor


{-| Initializes an editor

    model <| State.state docNode Nothing

-}
init : State -> Editor
init =
    InternalEditor.editor


{-| The internal events that an editor has to respond to.
-}
type alias Message =
    InternalEditor.Message


{-| Retrieves the current state from the editor
-}
state : Editor -> State
state =
    InternalEditor.state


{-| Retrieves the current history from the editor
-}
history : Editor -> History
history =
    InternalEditor.history


{-| Retrieves the shortKey from the editor. Note that this gets updated after the editor has been
rendered.
-}
shortKey : Editor -> String
shortKey =
    InternalEditor.shortKey


{-| Sets the history on the editor.

    editor
        |> withHistory newHistory

-}
withHistory : History -> Editor -> Editor
withHistory =
    InternalEditor.withHistory


applyList : NamedCommandList -> Spec -> Editor -> Result String Editor
applyList =
    InternalEditor.applyNamedCommandList


apply : NamedCommand -> Spec -> Editor -> Result String Editor
apply =
    InternalEditor.applyCommand


applyNoForceSelection : NamedCommand -> Spec -> Editor -> Result String Editor
applyNoForceSelection =
    InternalEditor.applyCommandNoForceSelection
