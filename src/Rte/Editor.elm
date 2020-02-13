module Rte.Editor exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import List.Extra
import Rte.BeforeInput
import Rte.DOMNode exposing (decodeDOMNode, extractRootEditorBlockNode, findTextChanges)
import Rte.EditorUtils exposing (forceRerender, zeroWidthSpace)
import Rte.HtmlNode exposing (editorBlockNodeToHtmlNode)
import Rte.KeyDown
import Rte.Marks exposing (selectableMark)
import Rte.Model exposing (..)
import Rte.NodePath as NodePath
import Rte.NodeUtils exposing (NodeResult(..), findNode)
import Rte.Selection exposing (caretSelection, domToEditor, editorToDom, isCollapsed)
import Rte.Spec exposing (childNodesPlaceholder, findNodeDefinitionFromSpec)


updateSelection : Maybe Selection -> Editor msg -> Editor msg
updateSelection maybeSelection editor =
    let
        editorState =
            editor.editorState
    in
    case maybeSelection of
        Nothing ->
            { editor | editorState = { editorState | selection = maybeSelection } }

        Just selection ->
            { editor | editorState = { editorState | selection = domToEditor editor.spec editorState.root selection } }


internalUpdate : InternalEditorMsg -> Editor msg -> Editor msg
internalUpdate msg editor =
    case msg of
        ChangeEvent change ->
            updateChangeEvent change editor

        SelectionEvent selection ->
            updateSelection selection editor

        BeforeInputEvent inputEvent ->
            Rte.BeforeInput.handleBeforeInput inputEvent editor

        CompositionStart ->
            handleCompositionStart editor

        CompositionEnd ->
            handleCompositionEnd editor

        KeyDownEvent e ->
            Rte.KeyDown.handleKeyDown e editor


textChanges : Spec -> EditorBlockNode -> DOMNode -> Maybe (List TextChange)
textChanges spec editorNode domNode =
    let
        htmlNode =
            editorBlockNodeToHtmlNode spec editorNode
    in
    case findTextChanges htmlNode domNode of
        Nothing ->
            Nothing

        Just changes ->
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


applyForceRerenderEditor : (Editor msg -> Editor msg) -> Editor msg -> Editor msg
applyForceRerenderEditor rerenderFunc editor =
    rerenderFunc
        (case editor.bufferedEditorState of
            Nothing ->
                editor

            Just bufferedEditorState ->
                { editor | editorState = bufferedEditorState, bufferedEditorState = Nothing, isComposing = False }
        )


updateChangeEvent : EditorChange -> Editor msg -> Editor msg
updateChangeEvent change editor =
    case extractRootEditorBlockNode change.root of
        Nothing ->
            applyForceRerenderEditor forceCompleteRerender editor

        Just editorRootDOMNode ->
            if needCompleteRerender change.root then
                applyForceRerenderEditor forceCompleteRerender editor

            else
                case textChanges editor.spec editor.editorState.root editorRootDOMNode of
                    Just changes ->
                        let
                            editorState =
                                editor.editorState
                        in
                        if List.isEmpty changes then
                            editor

                        else
                            case replaceText editorState.root changes of
                                Nothing ->
                                    applyForceRerenderEditor forceRerender editor

                                Just replacedEditorNodes ->
                                    let
                                        newEditorState =
                                            { editorState
                                                | selection = change.selection |> Maybe.andThen (domToEditor editor.spec editorState.root)
                                                , root = replacedEditorNodes
                                            }
                                    in
                                    if editor.isComposing then
                                        { editor | bufferedEditorState = Just newEditorState }

                                    else
                                        let
                                            newEditor =
                                                { editor | editorState = newEditorState }
                                        in
                                        applyForceRerenderEditor forceReselection newEditor

                    Nothing ->
                        applyForceRerenderEditor forceRerender editor


forceReselection : Editor msg -> Editor msg
forceReselection editor =
    { editor | selectionCount = editor.selectionCount + 1 }


needCompleteRerender : DOMNode -> Bool
needCompleteRerender root =
    case root of
        DOMNode v ->
            let
                cnodes =
                    Maybe.withDefault [] v.childNodes
            in
            List.length cnodes /= 1


editorChangeDecoder : D.Decoder InternalEditorMsg
editorChangeDecoder =
    D.map ChangeEvent
        (D.map2 EditorChange
            (D.at [ "detail", "root" ] decodeDOMNode)
            (D.at [ "detail", "selection" ] selectionDecoder)
        )


onEditorChange : (InternalEditorMsg -> msg) -> Html.Attribute msg
onEditorChange msgFunc =
    Html.Events.on "editorchange" (D.map msgFunc editorChangeDecoder)


selectionDecoder : D.Decoder (Maybe Selection)
selectionDecoder =
    D.maybe
        (D.map4 Selection
            (D.at [ "anchorOffset" ] D.int)
            (D.at [ "anchorNode" ] (D.list D.int))
            (D.at [ "focusOffset" ] D.int)
            (D.at [ "focusNode" ] (D.list D.int))
        )


editorSelectionChangeDecoder : D.Decoder InternalEditorMsg
editorSelectionChangeDecoder =
    D.map SelectionEvent
        (D.at [ "detail" ] selectionDecoder)


onCompositionStart : (InternalEditorMsg -> msg) -> Html.Attribute msg
onCompositionStart msgFunc =
    Html.Events.on "compositionstart" (D.map msgFunc (D.succeed CompositionStart))


onCompositionEnd : (InternalEditorMsg -> msg) -> Html.Attribute msg
onCompositionEnd msgFunc =
    Html.Events.on "compositionend" (D.map msgFunc (D.succeed CompositionEnd))


onEditorSelectionChange : (InternalEditorMsg -> msg) -> Html.Attribute msg
onEditorSelectionChange msgFunc =
    Html.Events.on "editorselectionchange" (D.map msgFunc editorSelectionChangeDecoder)


replaceText : EditorBlockNode -> List TextChange -> Maybe EditorBlockNode
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


applyTextChange : EditorBlockNode -> TextChange -> Maybe EditorBlockNode
applyTextChange editorNode ( path, text ) =
    case path of
        [] ->
            Nothing

        x :: xs ->
            case editorNode.childNodes of
                BlockList list ->
                    case List.Extra.getAt x list of
                        Nothing ->
                            Nothing

                        Just cblock ->
                            case applyTextChange cblock ( xs, text ) of
                                Nothing ->
                                    Nothing

                                Just textChangeNode ->
                                    Just { editorNode | childNodes = BlockList <| List.Extra.setAt x textChangeNode list }

                InlineLeafList list ->
                    if not <| List.isEmpty xs then
                        Nothing

                    else
                        case List.Extra.getAt x list of
                            Nothing ->
                                Nothing

                            Just inlineNode ->
                                case inlineNode of
                                    TextLeaf contents ->
                                        Just { editorNode | childNodes = InlineLeafList <| List.Extra.setAt x (TextLeaf { contents | text = text }) list }

                                    _ ->
                                        Nothing

                Leaf ->
                    Nothing



-- First unwrap the editor node


serializeNodePath : NodePath -> String
serializeNodePath nodePath =
    String.join ":" (List.map String.fromInt nodePath)


selectionAttribute : Maybe Selection -> Int -> Int -> String
selectionAttribute maybeSelection renderCount selectionCount =
    case maybeSelection of
        Nothing ->
            "render-count=" ++ String.fromInt renderCount

        Just selection ->
            String.join ","
                [ "anchor-offset=" ++ String.fromInt selection.anchorOffset
                , "anchor-node=" ++ serializeNodePath selection.anchorNode
                , "focus-offset=" ++ String.fromInt selection.focusOffset
                , "focus-node=" ++ serializeNodePath selection.focusNode
                , "render-count=" ++ String.fromInt renderCount
                , "selection-count=" ++ String.fromInt selectionCount
                ]


onBeforeInput : InputEventTypeMap -> EditorState -> (InternalEditorMsg -> msg) -> Html.Attribute msg
onBeforeInput inputEventTypeMap editorState msgFunc =
    Html.Events.preventDefaultOn "beforeinput" (Rte.BeforeInput.preventDefaultOnBeforeInputDecoder inputEventTypeMap editorState msgFunc)


onKeyDown : KeyMap -> EditorState -> (InternalEditorMsg -> msg) -> Html.Attribute msg
onKeyDown keyMap editorState msgFunc =
    Html.Events.preventDefaultOn "keydown" (Rte.KeyDown.preventDefaultOnKeyDownDecoder keyMap editorState msgFunc)


handleCompositionStart : Editor msg -> Editor msg
handleCompositionStart editor =
    { editor | isComposing = True }


handleCompositionEnd : Editor msg -> Editor msg
handleCompositionEnd editor =
    let
        ( newEditorState, rerender ) =
            case editor.bufferedEditorState of
                Nothing ->
                    ( editor.editorState, False )

                Just editorState ->
                    ( editorState, True )
    in
    if rerender then
        forceRerender { editor | editorState = newEditorState, isComposing = False, bufferedEditorState = Nothing }

    else
        { editor | isComposing = False }


readOnlyAttribute =
    StringAttribute "contenteditable" "false"


shouldHideCaret : EditorState -> Bool
shouldHideCaret editorState =
    case editorState.selection of
        Nothing ->
            True

        Just selection ->
            if not <| isCollapsed selection then
                False

            else
                case findNode selection.anchorNode editorState.root of
                    NoResult ->
                        False

                    BlockNodeResult blockNode ->
                        True

                    InlineLeafResult leaf ->
                        case leaf of
                            InlineLeaf params ->
                                True

                            _ ->
                                False


markCaretSelectionOnEditorNodes : EditorState -> EditorBlockNode
markCaretSelectionOnEditorNodes editorState =
    editorState.root


editorToDomSelection : Editor msg -> Maybe Selection
editorToDomSelection editor =
    case editor.editorState.selection of
        Nothing ->
            Nothing

        Just selection ->
            editorToDom editor.spec editor.editorState.root selection


renderEditor : Editor msg -> Html msg
renderEditor editor =
    Html.Keyed.node "elm-editor"
        [ onEditorChange editor.decoder
        , onEditorSelectionChange editor.decoder
        , onCompositionStart editor.decoder
        , onCompositionEnd editor.decoder
        ]
        [ ( String.fromInt editor.completeRerenderCount
          , Html.Keyed.node "div"
                [ Html.Attributes.contenteditable True
                , Html.Attributes.class "rte-main"
                , Html.Attributes.attribute "data-rte-main" "true"
                , Html.Attributes.classList [ ( "rte-hide-caret", shouldHideCaret editor.editorState ) ]
                , onBeforeInput editor.commandMap.inputEventTypeMap editor.editorState editor.decoder
                , onKeyDown editor.commandMap.keyMap editor.editorState editor.decoder
                ]
                [ ( String.fromInt editor.renderCount
                  , renderEditorBlockNode
                        editor.spec
                        editor.decoder
                        []
                        (markCaretSelectionOnEditorNodes editor.editorState)
                  )
                ]
          )
        , ( "selectionstate"
          , Html.node "selection-state" [ Html.Attributes.attribute "selection" (selectionAttribute (editorToDomSelection editor) editor.renderCount editor.selectionCount) ] []
          )
        ]


onClickSelect : DecoderFunc msg -> NodePath -> Html.Attribute msg
onClickSelect decoder nodePath =
    Html.Events.onClick (decoder <| SelectionEvent (Just (caretSelection nodePath 0)))


renderHtmlNode : HtmlNode -> List (Html msg) -> Html msg
renderHtmlNode node vdomChildren =
    case node of
        ElementNode name attributes children ->
            let
                childNodes =
                    if children == childNodesPlaceholder then
                        vdomChildren

                    else
                        List.map (\n -> renderHtmlNode n vdomChildren) children
            in
            Html.node
                name
                (List.map (\( k, v ) -> Html.Attributes.attribute k v) attributes)
                childNodes

        TextNode v ->
            Html.text v


renderMarkFromSpec : Spec -> NodePath -> Mark -> Html msg -> Html msg
renderMarkFromSpec spec backwardsNodePath mark child =
    case List.Extra.find (\m -> m.name == mark.name) spec.marks of
        Nothing ->
            child

        Just definition ->
            let
                node =
                    definition.toHtmlNode mark childNodesPlaceholder
            in
            renderHtmlNode node [ child ]


renderElementFromSpec : Spec -> ElementParameters -> NodePath -> List (Html msg) -> Html msg
renderElementFromSpec spec elementParameters backwardsNodePath children =
    let
        definition =
            findNodeDefinitionFromSpec elementParameters.name spec

        node =
            definition.toHtmlNode elementParameters childNodesPlaceholder

        nodeHtml =
            renderHtmlNode node children

        nodeHtmlWithMarks =
            List.foldr (renderMarkFromSpec spec backwardsNodePath) nodeHtml elementParameters.marks
    in
    nodeHtmlWithMarks


renderEditorBlockNode : Spec -> DecoderFunc msg -> NodePath -> EditorBlockNode -> Html msg
renderEditorBlockNode spec decoderFunc backwardsPath node =
    renderElementFromSpec spec
        node.parameters
        backwardsPath
        (case node.childNodes of
            BlockList l ->
                List.map (renderEditorBlockNode spec decoderFunc backwardsPath) l

            InlineLeafList l ->
                List.map (renderInlineLeaf spec decoderFunc backwardsPath) l

            Leaf ->
                []
        )


renderText : Spec -> TextLeafContents -> NodePath -> Html msg
renderText spec textLeafContents backwardsPath =
    let
        textHtml =
            Html.text
                (if String.isEmpty textLeafContents.text then
                    zeroWidthSpace

                 else
                    textLeafContents.text
                )
    in
    List.foldl (renderMarkFromSpec spec backwardsPath) textHtml textLeafContents.marks


renderInlineLeaf : Spec -> DecoderFunc msg -> NodePath -> EditorInlineLeaf -> Html msg
renderInlineLeaf spec decoderFunc backwardsPath leaf =
    case leaf of
        InlineLeaf elementParameters ->
            renderElementFromSpec spec elementParameters backwardsPath []

        TextLeaf v ->
            renderText spec v backwardsPath


forceCompleteRerender : Editor msg -> Editor msg
forceCompleteRerender editor =
    { editor | completeRerenderCount = editor.completeRerenderCount + 1 }
