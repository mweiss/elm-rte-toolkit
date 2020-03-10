module RichTextEditor.Editor exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import List.Extra
import RichTextEditor.BeforeInput
import RichTextEditor.Commands exposing (removeRangeSelection)
import RichTextEditor.Decorations exposing (getElementDecorators, getMarkDecorators)
import RichTextEditor.DomNode exposing (decodeDomNode, extractRootEditorBlockNode, findTextChanges)
import RichTextEditor.HtmlNode exposing (editorBlockNodeToHtmlNode)
import RichTextEditor.KeyDown
import RichTextEditor.Model.Editor exposing (Editor, state)
import RichTextEditor.Model.Selection exposing (Selection)
import RichTextEditor.Node exposing (nodeAt)
import RichTextEditor.NodePath as NodePath exposing (toString)
import RichTextEditor.Paste
import RichTextEditor.Selection exposing (annotateSelection, domToEditor)
import RichTextEditor.Spec exposing (childNodesPlaceholder, findNodeDefinitionFromSpecWithDefault)


updateSelection : Maybe Selection -> Bool -> Editor msg -> Editor msg
updateSelection maybeSelection isDomPath editor =
    let
        editorState =
            (state editor)
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
            RichTextEditor.BeforeInput.handleBeforeInput inputEvent editor

        CompositionStart ->
            handleCompositionStart editor

        CompositionEnd ->
            handleCompositionEnd editor

        KeyDownEvent e ->
            RichTextEditor.KeyDown.handleKeyDown e editor

        PasteWithDataEvent e ->
            RichTextEditor.Paste.handlePaste e editor

        CutEvent ->
            handleCut editor


handleCut : Editor msg -> Editor msg
handleCut editor =
    case applyNamedCommandList [ ( "removeRangeSelection", transformCommand removeRangeSelection ) ] editor of
        Err _ ->
            editor

        Ok e ->
            forceRerender e


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
                let
                    newEditor =
                        updateEditorState "buffered" bufferedEditorState editor
                in
                { newEditor
                    | bufferedEditorState = Nothing
                    , isComposing = False
                }
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


differentText : EditorBlockNode -> TextChange -> Bool
differentText root ( path, text ) =
    case nodeAt path root of
        Nothing ->
            True

        -- We'll mark invalid paths as different since it will resolve later when we try to replace the node
        Just node ->
            case node of
                InlineLeafWrapper il ->
                    case il of
                        TextLeaf tl ->
                            tl.text /= text

                        _ ->
                            True

                -- Again, invalid paths will be resolved later, so just mark it as true
                _ ->
                    True


updateChangeEventTextChanges : List TextChange -> Maybe Selection -> Editor msg -> Editor msg
updateChangeEventTextChanges textChanges selection editor =
    case textChangesDomToEditor editor.spec editor.editorState.root textChanges of
        Nothing ->
            applyForceFunctionOnEditor forceRerender editor

        Just changes ->
            let
                editorState =
                    editor.editorState

                actualChanges =
                    Debug.log "changes" (List.filter (differentText editorState.root) changes)
            in
            if List.isEmpty actualChanges then
                editor

            else
                case replaceText editorState.root actualChanges of
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
                                    updateEditorState "textChange" newEditorState editor
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
                BlockArray a ->
                    case Array.get x a of
                        Nothing ->
                            Nothing

                        Just cblock ->
                            case applyTextChange cblock ( xs, text ) of
                                Nothing ->
                                    Nothing

                                Just textChangeNode ->
                                    Just { editorNode | childNodes = BlockArray <| Array.set x textChangeNode a }

                InlineLeafArray a ->
                    if not <| List.isEmpty xs then
                        Nothing

                    else
                        case Array.get x a.array of
                            Nothing ->
                                Nothing

                            Just inlineNode ->
                                case inlineNode of
                                    TextLeaf contents ->
                                        Just { editorNode | childNodes = inlineLeafArray <| Array.set x (TextLeaf { contents | text = String.replace zeroWidthSpace "" text }) a.array }

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
    Html.Events.preventDefaultOn "beforeinput" (RichTextEditor.BeforeInput.preventDefaultOnBeforeInputDecoder editor)


onKeyDown : Editor msg -> Html.Attribute msg
onKeyDown editor =
    Html.Events.preventDefaultOn "keydown" (RichTextEditor.KeyDown.preventDefaultOnKeyDownDecoder editor)


handleCompositionStart : Editor msg -> Editor msg
handleCompositionStart editor =
    { editor | isComposing = True }


handleCompositionEnd : Editor msg -> Editor msg
handleCompositionEnd editor =
    case editor.bufferedEditorState of
        Nothing ->
            { editor | isComposing = False }

        Just _ ->
            applyForceFunctionOnEditor forceReselection editor


shouldHideCaret : EditorState -> Bool
shouldHideCaret editorState =
    case (State.selection editorState) of
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
    case (State.selection editorState) of
        Nothing ->
            editorState.root

        Just selection ->
            if isCollapsed selection then
                annotateSelection selection editorState.root

            else
                editorState.root


editorToDomSelection : Editor msg -> Maybe Selection
editorToDomSelection editor =
    case editor.(State.selection editorState) of
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
        , onPasteWithData editor.decoder
        , onCut editor.decoder
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


renderMarkFromSpec : Editor msg -> NodePath -> Mark -> Array (Html msg) -> Html msg
renderMarkFromSpec editor backwardsNodePath mark children =
    let
        markDecorators =
            getMarkDecorators mark.name editor.decorations

        decorators =
            List.map (\d -> d editor.decoder (List.reverse backwardsNodePath) mark) markDecorators
    in
    case List.Extra.find (\m -> m.name == mark.name) editor.spec.marks of
        Nothing ->
            Html.span [ Html.Attributes.class "rte-error" ] <| Array.toList children

        Just definition ->
            let
                node =
                    definition.toHtmlNode mark childNodesPlaceholder
            in
            renderHtmlNode node decorators children []


renderElementFromSpec : Editor msg -> ElementParameters -> NodePath -> Array (Html msg) -> Html msg
renderElementFromSpec editor elementParameters backwardsNodePath children =
    let
        definition =
            findNodeDefinitionFromSpecWithDefault elementParameters.name editor.spec

        node =
            definition.toHtmlNode elementParameters childNodesPlaceholder

        elementDecorators =
            getElementDecorators elementParameters.name editor.decorations

        decorators =
            List.map (\d -> d editor.decoder (List.reverse backwardsNodePath) elementParameters) elementDecorators

        nodeHtml =
            renderHtmlNode node decorators children []
    in
    nodeHtml


renderInlineLeafTree : Editor msg -> NodePath -> Array EditorInlineLeaf -> InlineLeafTree -> Html msg
renderInlineLeafTree editor backwardsPath inlineLeafArray inlineLeafTree =
    case inlineLeafTree of
        LeafNode i ->
            case Array.get i inlineLeafArray of
                Just l ->
                    renderInlineLeaf editor (i :: backwardsPath) l

                Nothing ->
                    -- TODO: Probably not the best thing, but what else can we do if we have an invalid tree?
                    Html.div [ Html.Attributes.class "rte-error" ] [ Html.text "Invalid leaf tree." ]

        MarkNode n ->
            renderMarkFromSpec editor backwardsPath n.mark <|
                Array.map (renderInlineLeafTree editor backwardsPath inlineLeafArray) n.children


renderEditorBlockNode : Editor msg -> NodePath -> EditorBlockNode -> Html msg
renderEditorBlockNode editor backwardsPath node =
    renderElementFromSpec editor
        node.parameters
        backwardsPath
        (case node.childNodes of
            BlockArray l ->
                Array.indexedMap (\i n -> renderEditorBlockNode editor (i :: backwardsPath) n) l

            InlineLeafArray l ->
                Array.map (\n -> renderInlineLeafTree editor backwardsPath l.array n) l.tree

            Leaf ->
                Array.empty
        )


renderText : String -> Html msg
renderText text =
    Html.text
        (if String.isEmpty text then
            zeroWidthSpace

         else
            text
        )


renderInlineLeaf : Editor msg -> NodePath -> EditorInlineLeaf -> Html msg
renderInlineLeaf editor backwardsPath leaf =
    case leaf of
        InlineLeaf l ->
            renderElementFromSpec editor l.parameters backwardsPath Array.empty

        TextLeaf v ->
            renderText v.text
