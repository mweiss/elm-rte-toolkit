module Rte.Editor exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import List.Extra
import Rte.BeforeInput
import Rte.Decorations exposing (getElementDecorators, getMarkDecorators)
import Rte.DomNode exposing (decodeDomNode, extractRootEditorBlockNode, findTextChanges)
import Rte.EditorUtils exposing (forceRerender, forceReselection, zeroWidthSpace)
import Rte.HtmlNode exposing (editorBlockNodeToHtmlNode)
import Rte.KeyDown
import Rte.Model exposing (..)
import Rte.Node exposing (EditorNode(..), nodeAt)
import Rte.NodePath as NodePath exposing (toString)
import Rte.Selection exposing (domToEditor, editorToDom, isCollapsed, markSelection)
import Rte.Spec exposing (childNodesPlaceholder, findNodeDefinitionFromSpec)


updateSelection : Maybe Selection -> Bool -> Editor msg -> Editor msg
updateSelection maybeSelection isDomPath editor =
    let
        editorState =
            editor.editorState
    in
    case maybeSelection of
        Nothing ->
            { editor | editorState = { editorState | selection = maybeSelection } }

        Just selection ->
            let
                translatedSelection =
                    if isDomPath then
                        domToEditor editor.spec editorState.root selection

                    else
                        Just selection
            in
            { editor | editorState = { editorState | selection = translatedSelection } }


internalUpdate : InternalEditorMsg -> Editor msg -> Editor msg
internalUpdate msg editor =
    case msg of
        ChangeEvent change ->
            updateChangeEvent change editor

        SelectionEvent selection isDomPath ->
            updateSelection selection isDomPath editor

        BeforeInputEvent inputEvent ->
            Rte.BeforeInput.handleBeforeInput inputEvent editor

        CompositionStart ->
            handleCompositionStart editor

        CompositionEnd ->
            handleCompositionEnd editor

        KeyDownEvent e ->
            Rte.KeyDown.handleKeyDown e editor


textChangesDomToEditor : Spec -> EditorBlockNode -> List TextChange -> Maybe (List TextChange)
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


deriveTextChanges : Spec -> EditorBlockNode -> DomNode -> Result String (List TextChange)
deriveTextChanges spec editorNode domNode =
    let
        htmlNode =
            editorBlockNodeToHtmlNode spec editorNode
    in
    findTextChanges htmlNode domNode


applyForceFunctionOnEditor : (Editor msg -> Editor msg) -> Editor msg -> Editor msg
applyForceFunctionOnEditor rerenderFunc editor =
    rerenderFunc
        (case editor.bufferedEditorState of
            Nothing ->
                editor

            Just bufferedEditorState ->
                { editor | editorState = bufferedEditorState, bufferedEditorState = Nothing, isComposing = False }
        )


updateChangeEvent : EditorChange -> Editor msg -> Editor msg
updateChangeEvent change editor =
    case change.characterDataMutations of
        Nothing ->
            case D.decodeValue decodeDomNode change.root of
                Err _ ->
                    editor

                Ok root ->
                    updateChangeEventFullScan root change.selection editor

        Just characterDataMutations ->
            updateChangeEventTextChanges characterDataMutations change.selection editor


updateChangeEventTextChanges : List TextChange -> Maybe Selection -> Editor msg -> Editor msg
updateChangeEventTextChanges textChanges selection editor =
    case textChangesDomToEditor editor.spec editor.editorState.root textChanges of
        Nothing ->
            applyForceFunctionOnEditor forceRerender editor

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
                        applyForceFunctionOnEditor forceRerender editor

                    Just replacedEditorNodes ->
                        let
                            newEditorState =
                                { editorState
                                    | selection = selection |> Maybe.andThen (domToEditor editor.spec editorState.root)
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
                            applyForceFunctionOnEditor forceReselection newEditor


updateChangeEventFullScan : DomNode -> Maybe Selection -> Editor msg -> Editor msg
updateChangeEventFullScan domRoot selection editor =
    case extractRootEditorBlockNode domRoot of
        Nothing ->
            applyForceFunctionOnEditor forceCompleteRerender editor

        Just editorRootDomNode ->
            if needCompleteRerender domRoot then
                applyForceFunctionOnEditor forceCompleteRerender editor

            else
                case deriveTextChanges editor.spec editor.editorState.root editorRootDomNode of
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
        (D.map4 Selection
            (D.at [ "anchorOffset" ] D.int)
            (D.at [ "anchorNode" ] (D.list D.int))
            (D.at [ "focusOffset" ] D.int)
            (D.at [ "focusNode" ] (D.list D.int))
        )


editorSelectionChangeDecoder : D.Decoder InternalEditorMsg
editorSelectionChangeDecoder =
    D.map2 SelectionEvent
        (D.at [ "detail" ] selectionDecoder)
        (D.succeed True)


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
                BlockArray list ->
                    case Array.get x list of
                        Nothing ->
                            Nothing

                        Just cblock ->
                            case applyTextChange cblock ( xs, text ) of
                                Nothing ->
                                    Nothing

                                Just textChangeNode ->
                                    Just { editorNode | childNodes = BlockArray <| Array.set x textChangeNode list }

                InlineLeafArray list ->
                    if not <| List.isEmpty xs then
                        Nothing

                    else
                        case Array.get x list of
                            Nothing ->
                                Nothing

                            Just inlineNode ->
                                case inlineNode of
                                    TextLeaf contents ->
                                        Just { editorNode | childNodes = InlineLeafArray <| Array.set x (TextLeaf { contents | text = String.replace zeroWidthSpace "" text }) list }

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
                [ "anchor-offset=" ++ String.fromInt selection.anchorOffset
                , "anchor-node=" ++ toString selection.anchorNode
                , "focus-offset=" ++ String.fromInt selection.focusOffset
                , "focus-node=" ++ toString selection.focusNode
                , "render-count=" ++ String.fromInt renderCount
                , "selection-count=" ++ String.fromInt selectionCount
                ]


onBeforeInput : Editor msg -> Html.Attribute msg
onBeforeInput editor =
    Html.Events.preventDefaultOn "beforeinput" (Rte.BeforeInput.preventDefaultOnBeforeInputDecoder editor)


onKeyDown : Editor msg -> Html.Attribute msg
onKeyDown editor =
    Html.Events.preventDefaultOn "keydown" (Rte.KeyDown.preventDefaultOnKeyDownDecoder editor)


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
                case nodeAt selection.anchorNode editorState.root of
                    Nothing ->
                        False

                    Just node ->
                        case node of
                            BlockNodeWrapper _ ->
                                True

                            InlineLeafWrapper leaf ->
                                case leaf of
                                    InlineLeaf _ ->
                                        True

                                    _ ->
                                        False


markCaretSelectionOnEditorNodes : EditorState -> EditorBlockNode
markCaretSelectionOnEditorNodes editorState =
    case editorState.selection of
        Nothing ->
            editorState.root

        Just selection ->
            if isCollapsed selection then
                markSelection selection editorState.root

            else
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
                , onBeforeInput editor
                , onKeyDown editor
                ]
                [ ( String.fromInt editor.renderCount
                  , renderEditorBlockNode
                        editor
                        []
                        (markCaretSelectionOnEditorNodes editor.editorState)
                  )
                ]
          )
        , ( "selectionstate"
          , Html.node "selection-state" [ Html.Attributes.attribute "selection" (selectionAttribute (editorToDomSelection editor) editor.renderCount editor.selectionCount) ] []
          )
        ]


renderHtmlNode : HtmlNode -> List (NodePath -> List (Html.Attribute msg)) -> Array (Html msg) -> NodePath -> Html msg
renderHtmlNode node decorators vdomChildren backwardsRelativePath =
    case node of
        ElementNode name attributes children ->
            let
                childNodes =
                    if children == childNodesPlaceholder then
                        vdomChildren

                    else
                        Array.indexedMap
                            (\i n -> renderHtmlNode n [] vdomChildren (i :: backwardsRelativePath))
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


renderMarkFromSpec : Editor msg -> NodePath -> Mark -> Html msg -> Html msg
renderMarkFromSpec editor backwardsNodePath mark child =
    let
        markDecorators =
            getMarkDecorators mark.name editor.decorations

        decorators =
            List.map (\d -> d editor.decoder (List.reverse backwardsNodePath) mark) markDecorators
    in
    case List.Extra.find (\m -> m.name == mark.name) editor.spec.marks of
        Nothing ->
            child

        Just definition ->
            let
                node =
                    definition.toHtmlNode mark childNodesPlaceholder
            in
            renderHtmlNode node decorators (Array.fromList [ child ]) []


renderElementFromSpec : Editor msg -> ElementParameters -> NodePath -> Array (Html msg) -> Html msg
renderElementFromSpec editor elementParameters backwardsNodePath children =
    let
        definition =
            findNodeDefinitionFromSpec elementParameters.name editor.spec

        node =
            definition.toHtmlNode elementParameters childNodesPlaceholder

        elementDecorators =
            getElementDecorators elementParameters.name editor.decorations

        decorators =
            List.map (\d -> d editor.decoder (List.reverse backwardsNodePath) elementParameters) elementDecorators

        nodeHtml =
            renderHtmlNode node decorators children []

        nodeHtmlWithMarks =
            List.foldl (renderMarkFromSpec editor backwardsNodePath) nodeHtml elementParameters.marks
    in
    nodeHtmlWithMarks


renderEditorBlockNode : Editor msg -> NodePath -> EditorBlockNode -> Html msg
renderEditorBlockNode editor backwardsPath node =
    renderElementFromSpec editor
        node.parameters
        backwardsPath
        (case node.childNodes of
            BlockArray l ->
                Array.indexedMap (\i n -> renderEditorBlockNode editor (i :: backwardsPath) n) l

            InlineLeafArray l ->
                Array.indexedMap (\i n -> renderInlineLeaf editor (i :: backwardsPath) n) l

            Leaf ->
                Array.empty
        )


renderText : Editor msg -> TextLeafContents -> NodePath -> Html msg
renderText editor textLeafContents backwardsPath =
    let
        textHtml =
            Html.text
                (if String.isEmpty textLeafContents.text then
                    zeroWidthSpace

                 else
                    textLeafContents.text
                )
    in
    List.foldl (renderMarkFromSpec editor backwardsPath) textHtml textLeafContents.marks


renderInlineLeaf : Editor msg -> NodePath -> EditorInlineLeaf -> Html msg
renderInlineLeaf editor backwardsPath leaf =
    case leaf of
        InlineLeaf elementParameters ->
            renderElementFromSpec editor elementParameters backwardsPath Array.empty

        TextLeaf v ->
            renderText editor v backwardsPath


forceCompleteRerender : Editor msg -> Editor msg
forceCompleteRerender editor =
    { editor | completeRerenderCount = editor.completeRerenderCount + 1 }
