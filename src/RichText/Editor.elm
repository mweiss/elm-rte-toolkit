module RichText.Editor exposing
    ( Editor, init, state, shortKey, history, withHistory, changeCount
    , Config, config, commandMap, decorations, spec
    , Message, update, apply, applyList, applyNoForceSelection
    , view, readOnlyView
    )

{-| This is the main module for an editor, and contains functions for initializing, updating, and
rendering an editor.


# Model

@docs Editor, init, state, shortKey, history, withHistory, changeCount


# Config

@docs Config, config, commandMap, decorations, spec


# Update

@docs Message, update, apply, applyList, applyNoForceSelection


# View

@docs view, readOnlyView

-}

import Array exposing (Array)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as D
import RichText.Annotation exposing (annotateSelection)
import RichText.Commands exposing (removeRange)
import RichText.Config.Command exposing (CommandMap, NamedCommand, NamedCommandList, transform)
import RichText.Config.Decorations exposing (Decorations, elementDecorations, markDecorations, topLevelAttributes)
import RichText.Config.ElementDefinition as ElementDefinition
import RichText.Config.MarkDefinition as MarkDefinition
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
import RichText.Internal.Editor as InternalEditor
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
        , updateEditorStateWithTimestamp
        , withBufferedEditorState
        , withComposing
        , withShortKey
        , withState
        )
import RichText.Internal.Event exposing (EditorChange, InitEvent, PasteEvent, TextChange)
import RichText.Internal.HtmlNode exposing (childNodesPlaceholder, editorBlockNodeToHtmlNode)
import RichText.Internal.KeyDown as KeyDown
import RichText.Internal.Paste as Paste
import RichText.Internal.Path as NodePath
import RichText.Internal.Selection exposing (domToEditor, editorToDom)
import RichText.Internal.Spec exposing (elementDefinitionWithDefault, markDefinitionWithDefault)
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


{-| This type represents your Editor configuration, e.g. the non-comparable things that define
the behavior of the editor. This includes the document specification, key and input event command
bindings, decorative functions, and tagger function.
-}
type Config msg
    = Config
        { decorations : Decorations msg
        , spec : Spec
        , commandMap : CommandMap
        , toMsg : Message -> msg
        }


{-| Create the config for your `view` and `update` functions.

    import RichText.Commands exposing (defaultCommandMap)
    import RichText.Config.Decorations exposing (emptyDecorations)
    import RichText.Definitions exposing (markdown)

    type MyMsg
        = InternalMsg Message | ...

    myConfig : Config
    myConfig =
        config
            { decorations = emptyDecorations
            , commandMap = defaultCommandMap
            , spec = markdown
            , toMsg = InternalMsg
            }

-}
config :
    { decorations : Decorations msg
    , spec : Spec
    , commandMap : CommandMap
    , toMsg : Message -> msg
    }
    -> Config msg
config cfg =
    Config cfg


{-| The decorations from the config object.
-}
decorations : Config msg -> Decorations msg
decorations cfg =
    case cfg of
        Config c ->
            c.decorations


{-| The spec from the config object.
-}
spec : Config msg -> Spec
spec cfg =
    case cfg of
        Config c ->
            c.spec


{-| The commandMap from the config object.
-}
commandMap : Config msg -> CommandMap
commandMap cfg =
    case cfg of
        Config c ->
            c.commandMap


updateSelection : Maybe Selection -> Spec -> Editor -> Editor
updateSelection maybeSelection spec_ editor_ =
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
                    domToEditor spec_ (State.root editorState) selection
            in
            if isComposing editor_ then
                let
                    bufferedState =
                        Maybe.withDefault editorState (bufferedEditorState editor_)
                in
                editor_ |> withBufferedEditorState (Just (bufferedState |> withSelection translatedSelection))

            else
                editor_ |> withState (editorState |> withSelection translatedSelection)


selectElement : Path -> Spec -> Editor -> Editor
selectElement path _ editor_ =
    let
        editorState =
            state editor_

        selection =
            case RichText.Node.next path (State.root editorState) of
                Just ( b, _ ) ->
                    range b 0 path 0

                Nothing ->
                    RichText.Model.Selection.caret path 0
    in
    editor_ |> withState (editorState |> withSelection (Just selection)) |> forceReselection


{-| The editor's internal update function. It's important that the editor process all `Message`
events with the update function so it doesn't go out of sync with the virtual DOM.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            EditorMsg editorMsg ->
                ( { model | editor = RichText.Editor.update config editorMsg model.editor }, Cmd.none )

-}
update : Config msg -> Message -> Editor -> Editor
update cfg msg editor_ =
    case cfg of
        Config c ->
            let
                spec_ =
                    c.spec

                commandMap_ =
                    c.commandMap
            in
            case msg of
                ChangeEvent change ->
                    updateChangeEvent change spec_ editor_

                SelectionEvent selection ->
                    updateSelection selection spec_ editor_

                SelectElement path ->
                    selectElement path spec_ editor_

                BeforeInputEvent inputEvent ->
                    BeforeInput.handleBeforeInput inputEvent commandMap_ spec_ editor_

                CompositionStart ->
                    handleCompositionStart editor_

                CompositionEnd ->
                    handleCompositionEnd editor_

                KeyDownEvent e ->
                    KeyDown.handleKeyDown e commandMap_ spec_ editor_

                PasteWithDataEvent e ->
                    Paste.handlePaste e spec_ editor_

                CutEvent ->
                    handleCut spec_ editor_

                Init e ->
                    handleInitEvent e editor_


handleInitEvent : InitEvent -> Editor -> Editor
handleInitEvent initEvent editor_ =
    editor_ |> withShortKey initEvent.shortKey


handleCut : Spec -> Editor -> Editor
handleCut spec_ editor_ =
    case applyList [ ( "removeRangeSelection", transform removeRange ) ] spec_ editor_ of
        Err _ ->
            editor_

        Ok e ->
            forceRerender e


textChangesDomToEditor : Spec -> Block -> List TextChange -> Maybe (List TextChange)
textChangesDomToEditor spec_ editorNode changes =
    List.foldl
        (\( p, text ) maybeAgg ->
            case maybeAgg of
                Nothing ->
                    Nothing

                Just agg ->
                    case NodePath.domToEditor spec_ editorNode p of
                        Nothing ->
                            Nothing

                        Just translatedPath ->
                            Just (( translatedPath, text ) :: agg)
        )
        (Just [])
        changes


deriveTextChanges : Spec -> Block -> DomNode -> Result String (List TextChange)
deriveTextChanges spec_ editorNode domNode =
    let
        htmlNode =
            editorBlockNodeToHtmlNode spec_ editorNode
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
updateChangeEvent change spec_ editor_ =
    case change.characterDataMutations of
        Nothing ->
            case D.decodeValue decodeDomNode change.root of
                Err _ ->
                    editor_

                Ok root ->
                    updateChangeEventFullScan change.timestamp change.isComposing root change.selection spec_ editor_

        Just characterDataMutations ->
            updateChangeEventTextChanges
                change.timestamp
                change.isComposing
                (sanitizeMutations characterDataMutations)
                change.selection
                spec_
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


updateChangeEventTextChanges : Int -> Bool -> List TextChange -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventTextChanges timestamp composing textChanges selection spec_ editor_ =
    let
        editorComposing =
            composing || isComposing editor_

        -- Fix to issue #4: when composing text, we want to do the text comparison with the
        -- buffered state if it exists.
        stateToCompare =
            if editorComposing then
                Maybe.withDefault (state editor_) (bufferedEditorState editor_)

            else
                state editor_
    in
    case textChangesDomToEditor spec_ (State.root stateToCompare) textChanges of
        Nothing ->
            applyForceFunctionOnEditor forceRerender editor_

        Just changes ->
            let
                editorState =
                    state editor_

                actualChanges =
                    List.filter (differentText (State.root stateToCompare)) changes
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
                                    |> withSelection (selection |> Maybe.andThen (domToEditor spec_ (State.root editorState)))
                                    |> withRoot replacedEditorNodes
                        in
                        if editorComposing then
                            editor_
                                |> withBufferedEditorState (Just newEditorState)

                        else
                            let
                                newEditor =
                                    updateEditorStateWithTimestamp (Just timestamp) "textChange" newEditorState editor_
                            in
                            applyForceFunctionOnEditor forceReselection newEditor


updateChangeEventFullScan : Int -> Bool -> DomNode -> Maybe Selection -> Spec -> Editor -> Editor
updateChangeEventFullScan timestamp isComposing domRoot selection spec_ editor_ =
    case extractRootEditorBlockNode domRoot of
        Nothing ->
            applyForceFunctionOnEditor forceCompleteRerender editor_

        Just editorRootDomNode ->
            if needCompleteRerender domRoot then
                applyForceFunctionOnEditor forceCompleteRerender editor_

            else
                case deriveTextChanges spec_ (State.root (state editor_)) editorRootDomNode of
                    Ok changes ->
                        updateChangeEventTextChanges timestamp isComposing changes selection spec_ editor_

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
        (D.map5 EditorChange
            (D.at [ "detail", "root" ] D.value)
            (D.at [ "detail", "selection" ] selectionDecoder)
            (D.maybe (D.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder))
            (D.at [ "detail", "timestamp" ] D.int)
            (D.at [ "detail", "isComposing" ] (D.oneOf [ D.bool, D.succeed False ]))
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
    D.map SelectionEvent
        (D.at [ "detail" ] selectionDecoder)


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
    Html.Events.on "editorcompositionend" (D.map msgFunc (D.succeed CompositionEnd))


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
onBeforeInput tagger commandMap_ spec_ editor_ =
    Html.Events.preventDefaultOn "beforeinput" (BeforeInput.preventDefaultOnBeforeInputDecoder tagger commandMap_ spec_ editor_)


onKeyDown : Tagger msg -> CommandMap -> Spec -> Editor -> Html.Attribute msg
onKeyDown tagger commandMap_ spec_ editor_ =
    Html.Events.preventDefaultOn "keydown" (KeyDown.preventDefaultOnKeyDownDecoder tagger commandMap_ spec_ editor_)


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
editorToDomSelection spec_ editor_ =
    case State.selection (state editor_) of
        Nothing ->
            Nothing

        Just selection ->
            editorToDom spec_ (State.root (state editor_)) selection


{-| Take an editor model and config and render it in the DOM.
-}
view : Config msg -> Editor -> Html msg
view cfg editor_ =
    case cfg of
        Config c ->
            let
                tagger =
                    c.toMsg

                commandMap_ =
                    c.commandMap

                decorations_ =
                    c.decorations

                spec_ =
                    c.spec

                state_ =
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
                        ([ Html.Attributes.contenteditable True
                         , Html.Attributes.class "rte-main"
                         , Html.Attributes.attribute "data-rte-main" "true"
                         , Html.Attributes.classList [ ( "rte-hide-caret", shouldHideCaret state_ ) ]
                         , onBeforeInput tagger commandMap_ spec_ editor_
                         , onKeyDown tagger commandMap_ spec_ editor_
                         ]
                            ++ topLevelAttributes decorations_
                        )
                        [ ( String.fromInt (renderCount editor_)
                          , viewEditorBlockNode
                                spec_
                                decorations_
                                []
                                (markCaretSelectionOnEditorNodes state_)
                          )
                        ]
                  )
                , ( "selectionstate"
                  , Html.node "selection-state"
                        [ Html.Attributes.attribute
                            "selection"
                            (selectionAttribute
                                (editorToDomSelection spec_ editor_)
                                (renderCount editor_)
                                (selectionCount editor_)
                            )
                        ]
                        []
                  )
                ]


{-| Renders the contents of the editor with `contenteditable` set to false and the event listeners
removed.
-}
readOnlyView : Config msg -> Editor -> Html msg
readOnlyView cfg editor_ =
    case cfg of
        Config c ->
            let
                decorations_ =
                    c.decorations

                spec_ =
                    c.spec

                state_ =
                    state editor_
            in
            Html.node "div"
                ([ Html.Attributes.class "rte-main"
                 , Html.Attributes.attribute "data-rte-main" "true"
                 ]
                    ++ topLevelAttributes decorations_
                )
                [ viewEditorBlockNode
                    spec_
                    decorations_
                    []
                    (markCaretSelectionOnEditorNodes state_)
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
viewMark spec_ decorations_ backwardsNodePath mark children =
    let
        mDecorators =
            Maybe.withDefault []
                (Dict.get
                    (Mark.name mark)
                    (markDecorations decorations_)
                )

        decorators =
            List.map (\d -> d (List.reverse backwardsNodePath) mark) mDecorators

        node =
            MarkDefinition.toHtmlNode (markDefinitionWithDefault mark spec_) mark childNodesPlaceholder
    in
    viewHtmlNode node decorators children []


viewElement : Spec -> Decorations msg -> Element -> Path -> Array (Html msg) -> Html msg
viewElement spec_ decorations_ elementParameters backwardsNodePath children =
    let
        definition =
            elementDefinitionWithDefault elementParameters spec_

        node =
            ElementDefinition.toHtmlNode definition elementParameters childNodesPlaceholder

        eDecorators =
            Maybe.withDefault []
                (Dict.get
                    (Element.name elementParameters)
                    (elementDecorations decorations_)
                )

        decorators =
            List.map (\d -> d (List.reverse backwardsNodePath) elementParameters) eDecorators

        nodeHtml =
            viewHtmlNode node decorators children []
    in
    nodeHtml


viewInlineLeafTree : Spec -> Decorations msg -> Path -> Array Inline -> InlineTree -> Html msg
viewInlineLeafTree spec_ decorations_ backwardsPath inlineLeafArray inlineLeafTree =
    case inlineLeafTree of
        LeafNode i ->
            case Array.get i inlineLeafArray of
                Just l ->
                    viewInlineLeaf spec_ decorations_ (i :: backwardsPath) l

                Nothing ->
                    -- Not the best thing, but what else can we do if we have an invalid tree?
                    -- This state should be impossible though.
                    Html.div [ Html.Attributes.class "rte-error" ] [ Html.text "Invalid leaf tree." ]

        MarkNode n ->
            viewMark spec_ decorations_ backwardsPath n.mark <|
                Array.map (viewInlineLeafTree spec_ decorations_ backwardsPath inlineLeafArray) n.children


viewEditorBlockNode : Spec -> Decorations msg -> Path -> Block -> Html msg
viewEditorBlockNode spec_ decorations_ backwardsPath node =
    viewElement spec_
        decorations_
        (element node)
        backwardsPath
        (case childNodes node of
            BlockChildren l ->
                Array.indexedMap (\i n -> viewEditorBlockNode spec_ decorations_ (i :: backwardsPath) n) (toBlockArray l)

            InlineChildren l ->
                Array.map (\n -> viewInlineLeafTree spec_ decorations_ backwardsPath (toInlineArray l) n) (toInlineTree l)

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
viewInlineLeaf spec_ decorations_ backwardsPath leaf =
    case leaf of
        InlineElement l ->
            viewElement spec_ decorations_ (InlineElement.element l) backwardsPath Array.empty

        Text v ->
            viewText (Text.text v)


{-| `Editor` represents the entire state of the editor, and is what you store in your model.
-}
type alias Editor =
    InternalEditor.Editor


{-| Initializes an editor

    docNode : Block
    docNode =
        block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "Hello world" ])
                    ]
            )

    init <| State.state docNode Nothing

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


{-| The editor shortKey is a platform dependent key for command map bindings. It is initialized
to either `"Control"` or `"Meta"` depending on if the editor webcomponent detects if the platform
is mac/iOS or something else. Note that this gets updated after the editor has been rendered, and
defaults to `"Meta"`.
-}
shortKey : Editor -> String
shortKey =
    InternalEditor.shortKey


{-| Change count is a counter that gets incremented any time the editor's state gets updated. You
may want to use this as a quick way to see if the editor has changed via a command after the
`update` function. Note: this is a stop gap until a good story for how programmers can react to
editor state changes has been thought out.
-}
changeCount : Editor -> Int
changeCount =
    InternalEditor.changeCount


{-| Sets the history on the editor.

    editor
        |> withHistory newHistory

-}
withHistory : History -> Editor -> Editor
withHistory =
    InternalEditor.withHistory


{-| Apply a list of named commands to the editor to try in order, returning the updated editor after
the first command has been successful. If no command was successful, a String describing the last
command's error is returned.

This method stops execution of the commands after the first success. Its intent is to
allow you to group your commands for different contexts, like lift, join, split,
into one chained command. If you want multiple commands to be executed, you may want to compose
the respective transform functions or call apply for each command.

As with the `apply` command, each command is validated after it is applied, and if successful,
the editor state is reduced and the history is updated.

    liftBlock : Spec -> Model -> Model
    liftBlock spec model =
        { model
            | editor =
                Result.withDefault model.editor
                    (applyList
                        [ ( "liftList"
                          , transform <| RichText.List.lift defaultListDefinition
                          )
                        , ( "lift"
                          , transform <| lift
                          )
                        ]
                        spec
                        model.editor
                    )
        }

-}
applyList : NamedCommandList -> Spec -> Editor -> Result String Editor
applyList =
    InternalEditor.applyNamedCommandList


{-| Apply a named command to the editor. If the command was successful, the resulting editor
will be returned, otherwise a String describing the command's error is returned.

Note that after the command is executed, it is validated against the spec. If it is not valid, then
an error is returned. If the command is successful and validated, the resulting editor state is reduced
(via `RichText.State.reduce`) and the history is updated.

    wrapBlockNode : Spec -> Model -> Model
    wrapBlockNode spec model =
        { model
            | editor =
                Result.withDefault model.editor
                    (apply
                        ( "wrapBlockquote"
                        , transform <|
                            wrap
                                identity
                                (element blockquote [])
                        )
                        spec
                        model.editor
                    )
        }

-}
apply : NamedCommand -> Spec -> Editor -> Result String Editor
apply =
    InternalEditor.applyCommand


{-| Same as `apply`, but the selection state is not forced to update if it hasn't changed. This normally
should not be used, but can be useful for situations like if you have an embedded input element in a
"contentediable=false" wrapper that requires focus or independent selection.
-}
applyNoForceSelection : NamedCommand -> Spec -> Editor -> Result String Editor
applyNoForceSelection =
    InternalEditor.applyCommandNoForceSelection
