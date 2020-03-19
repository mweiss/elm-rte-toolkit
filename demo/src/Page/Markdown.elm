module Page.Markdown exposing (..)

import Array exposing (Array)
import Controls exposing (Style(..))
import Editor
import Html exposing (Html, a, div, h1, p, text, textarea)
import Html.Attributes exposing (href, title)
import Html.Events
import Links exposing (rteToolkit)
import Markdown.Block as M
import Markdown.Config as M
import Markdown.Inline as MI
import RichTextEditor.Model.Attribute
    exposing
        ( Attribute(..)
        , findIntegerAttribute
        , findStringAttribute
        )
import RichTextEditor.Model.Editor exposing (Editor, state)
import RichTextEditor.Model.Mark as Mark exposing (Mark, MarkOrder)
import RichTextEditor.Model.Node as Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , ElementParameters
        , InlineLeaf(..)
        , InlineLeafTree(..)
        , attributesFromElementParameters
        , blockArray
        , blockNode
        , childNodes
        , elementParameters
        , elementParametersFromBlockNode
        , elementParametersFromInlineLeafParameters
        , fromBlockArray
        , fromInlineArray
        , inlineLeaf
        , inlineLeafArray
        , nameFromElementParameters
        , textLeaf
        , textLeafWithText
        , treeFromInlineArray
        )
import RichTextEditor.Model.Spec exposing (Spec, withMarkDefinitions)
import RichTextEditor.Model.State as State exposing (State)
import RichTextEditor.Spec exposing (markOrderFromSpec)
import RichTextEditor.Specs as MarkdownSpec
    exposing
        ( blockquote
        , bold
        , code
        , codeBlock
        , doc
        , hardBreak
        , heading
        , horizontalRule
        , image
        , link
        , listItem
        , orderedList
        , paragraph
        , unorderedList
        )
import Session exposing (Session)
import Set


type alias CustomInline =
    {}


type alias CustomBlock =
    {}


type alias Block =
    M.Block CustomBlock CustomInline


type alias Inline =
    MI.Inline CustomInline


type EditorType
    = Markdown
    | WYSIWYG


type alias Model =
    { session : Session
    , editor : Editor.Model
    , textMarkdown : String
    , markdownError : Maybe String
    , editorType : EditorType
    }


customMarkdownSpec : Spec
customMarkdownSpec =
    MarkdownSpec.markdown
        |> withMarkDefinitions
            [ link
            , bold
            , code
            ]


type Msg
    = EditorMsg Editor.EditorMsg
    | EditorChange EditorType
    | TextAreaChange String
    | GotSession Session


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Markdown"
    , content =
        [ h1 [] [ text "Markdown example" ]
        , p []
            [ text """This is a markdown example."""
            ]
        , p []
            [ text "You can see the code for this example in the "
            , a
                [ title "git repo"
                , href (rteToolkit ++ "/tree/master/demo/src/Page/Markdown.elm")
                ]
                [ text "git repo." ]
            ]
        , markdownOrEditorView model
        ]
    }


markdownOrEditorView : Model -> Html Msg
markdownOrEditorView model =
    let
        editor =
            if model.editorType == WYSIWYG then
                Html.map EditorMsg (Editor.view model.editor)

            else
                markdownTextArea model
    in
    div []
        [ div []
            [ Html.input
                [ Html.Attributes.type_ "radio"
                , Html.Attributes.name "editorView"
                , Html.Events.onClick (EditorChange WYSIWYG)
                , Html.Attributes.checked (model.editorType == WYSIWYG)
                ]
                []
            , Html.label
                [ Html.Attributes.for "editorView" ]
                [ Html.text "WYSIWYG" ]
            , Html.input
                [ Html.Attributes.type_ "radio"
                , Html.Attributes.name "editorView"
                , Html.Events.onClick (EditorChange Markdown)
                , Html.Attributes.checked (model.editorType == Markdown)
                ]
                []
            , Html.label
                [ Html.Attributes.for "editorView" ]
                [ Html.text "Markdown" ]
            ]
        , div [ Html.Attributes.class "editor-error" ]
            (Maybe.withDefault []
                (Maybe.map (\x -> [ text x ]) model.markdownError)
            )
        , div
            [ Html.Attributes.class "switch-editor-container" ]
            [ editor ]
        ]


markdownTextArea : Model -> Html Msg
markdownTextArea model =
    div []
        [ textarea
            [ Html.Attributes.class "markdown-textarea"
            , Html.Attributes.value model.textMarkdown
            , Html.Events.onInput TextAreaChange
            ]
            []
        ]


init : Session -> ( Model, Cmd Msg )
init session =
    let
        markdownNodes =
            rootToMarkdown (State.root Editor.initialState)

        ( result, error ) =
            case Result.andThen markdownToString markdownNodes of
                Err e ->
                    ( "", Just e )

                Ok m ->
                    ( m, Nothing )
    in
    ( { session = session
      , editor = initializeEditor Editor.initialState
      , textMarkdown = result
      , markdownError = error
      , editorType = WYSIWYG
      }
    , Cmd.none
    )


markdownMarkOrder : MarkOrder
markdownMarkOrder =
    markOrderFromSpec customMarkdownSpec


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( e, _ ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = e }, Cmd.none )

        EditorChange type_ ->
            ( changeEditorType type_ model, Cmd.none )

        TextAreaChange value ->
            ( { model | textMarkdown = value }, Cmd.none )

        _ ->
            ( model, Cmd.none )


changeEditorType : EditorType -> Model -> Model
changeEditorType type_ model =
    if type_ == model.editorType then
        model

    else
        case type_ of
            WYSIWYG ->
                changeEditorTypeToWYSIWYG model

            Markdown ->
                changeEditorTypeToMarkdown model


changeEditorTypeToMarkdown : Model -> Model
changeEditorTypeToMarkdown model =
    let
        markdownNodes =
            rootToMarkdown (State.root (state model.editor.editor))

        ( result, error ) =
            case Result.andThen markdownToString markdownNodes of
                Err e ->
                    ( model.textMarkdown, Just e )

                Ok m ->
                    ( m, Nothing )
    in
    case error of
        Just e ->
            { model | markdownError = Just e }

        Nothing ->
            { model
                | textMarkdown = result
                , markdownError = Nothing
                , editorType = Markdown
            }


changeEditorTypeToWYSIWYG : Model -> Model
changeEditorTypeToWYSIWYG model =
    let
        markdownNodes =
            M.parse
                (Just
                    { softAsHardLineBreak = False
                    , rawHtml = M.DontParse
                    }
                )
                model.textMarkdown

        result =
            markdownToBlock markdownNodes
    in
    case result of
        Err e ->
            { model | markdownError = Just e }

        Ok root ->
            { model
                | editor = initializeEditor (State.state root Nothing)
                , markdownError = Nothing
                , editorType = WYSIWYG
            }


initializeEditor : State -> Editor.Model
initializeEditor state =
    let
        initialEditor =
            Editor.init state
                MarkdownSpec.markdown
    in
    { initialEditor | styles = [ Bold ] }


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


unwrapAndFilterChildNodes : List (Result String a) -> Result String (List a)
unwrapAndFilterChildNodes results =
    let
        unwrappedResults =
            List.filterMap
                (\x ->
                    case x of
                        Ok v ->
                            Just v

                        _ ->
                            Nothing
                )
                results
    in
    if List.length unwrappedResults == List.length results then
        Ok unwrappedResults

    else
        Err <|
            String.join "\n" <|
                List.filterMap
                    (\x ->
                        case x of
                            Err s ->
                                Just s

                            _ ->
                                Nothing
                    )
                    results


blockChildrenToMarkdown : ChildNodes -> Result String (List Block)
blockChildrenToMarkdown cn =
    case cn of
        BlockChildren a ->
            let
                results =
                    List.map blockToMarkdown (Array.toList (fromBlockArray a))
            in
            unwrapAndFilterChildNodes results

        InlineChildren _ ->
            Err "Invalid child nodes, received inline, expected block"

        Leaf ->
            Err "Invalid child nodes, received leaf, expected block"


inlineChildrenToMarkdown : ChildNodes -> Result String (List Inline)
inlineChildrenToMarkdown cn =
    case cn of
        InlineChildren a ->
            let
                results =
                    List.map (inlineToMarkdown (fromInlineArray a)) (Array.toList (treeFromInlineArray a))
            in
            Result.map (List.concatMap identity) (unwrapAndFilterChildNodes results)

        BlockChildren _ ->
            Err "Invalid child nodes, was expected inline, received block"

        Leaf ->
            Err "Invalid child nodes, was expected inline, received leaf"


rootToMarkdown : BlockNode -> Result String (List Block)
rootToMarkdown node =
    let
        children =
            childNodes node
    in
    blockChildrenToMarkdown children


imageToMarkdown : ElementParameters -> Result String Inline
imageToMarkdown parameters =
    let
        attributes =
            attributesFromElementParameters parameters

        alt =
            findStringAttribute "alt" attributes
    in
    case findStringAttribute "src" attributes of
        Nothing ->
            Err "No src attribute found"

        Just src ->
            Ok <| MI.Image src alt []


inlineToMarkdown : Array InlineLeaf -> InlineLeafTree -> Result String (List Inline)
inlineToMarkdown leaves tree =
    case tree of
        LeafNode i ->
            case Array.get i leaves of
                Nothing ->
                    Err "Invalid leaf tree"

                Just inlineLeaf ->
                    case inlineLeaf of
                        TextLeaf p ->
                            Ok <| [ MI.Text (Node.text p) ]

                        InlineLeaf il ->
                            let
                                parameters =
                                    elementParametersFromInlineLeafParameters il
                            in
                            case nameFromElementParameters parameters of
                                "image" ->
                                    Result.map List.singleton (imageToMarkdown parameters)

                                "hard_break" ->
                                    Ok <| [ MI.HardLineBreak ]

                                name ->
                                    Err <| "Unsupported inline leaf :" ++ name

        MarkNode m ->
            case unwrapAndFilterChildNodes <| List.map (inlineToMarkdown leaves) (Array.toList m.children) of
                Err s ->
                    Err s

                Ok children ->
                    let
                        flattenedChildren =
                            List.concatMap identity children
                    in
                    case Mark.name m.mark of
                        "bold" ->
                            Ok <| [ MI.Emphasis 2 flattenedChildren ]

                        "code" ->
                            Ok <|
                                List.map
                                    (\x ->
                                        case x of
                                            MI.Text s ->
                                                MI.CodeInline s

                                            _ ->
                                                x
                                    )
                                    flattenedChildren

                        "link" ->
                            let
                                attributes =
                                    Mark.attributes m.mark

                                title =
                                    findStringAttribute "title" attributes
                            in
                            case findStringAttribute "href" attributes of
                                Nothing ->
                                    Err "Invalid link mark"

                                Just href ->
                                    Ok <| [ MI.Link href title flattenedChildren ]

                        name ->
                            Err <| "Unsupported mark: " ++ name


textFromChildNodes : ChildNodes -> String
textFromChildNodes cn =
    case cn of
        InlineChildren il ->
            String.join "" <|
                Array.toList <|
                    Array.map
                        (\l ->
                            case l of
                                TextLeaf tl ->
                                    Node.text tl

                                InlineLeaf p ->
                                    if
                                        nameFromElementParameters
                                            (elementParametersFromInlineLeafParameters p)
                                            == "hard_break"
                                    then
                                        "\n"

                                    else
                                        ""
                        )
                        (fromInlineArray il)

        _ ->
            ""


headingToMarkdown : ElementParameters -> ChildNodes -> Result String Block
headingToMarkdown p cn =
    let
        attributes =
            attributesFromElementParameters p

        level =
            Maybe.withDefault 1 (findIntegerAttribute "level" attributes)
    in
    Result.map (M.Heading "" level) (inlineChildrenToMarkdown cn)


codeBlockToMarkdown : ChildNodes -> Result String Block
codeBlockToMarkdown cn =
    let
        t =
            textFromChildNodes cn
    in
    Ok <| M.CodeBlock M.Indented t


listToMarkdown : M.ListType -> ElementParameters -> ChildNodes -> Result String Block
listToMarkdown type_ parameters cn =
    let
        delimiter =
            Maybe.withDefault "." <|
                findStringAttribute
                    "delimiter"
                    (attributesFromElementParameters parameters)

        listItems =
            case cn of
                BlockChildren a ->
                    let
                        children =
                            Array.toList <| fromBlockArray a
                    in
                    unwrapAndFilterChildNodes <|
                        List.map
                            (\x ->
                                blockChildrenToMarkdown (childNodes x)
                            )
                            children

                _ ->
                    Err <| "Invalid list items"
    in
    case listItems of
        Err s ->
            Err s

        Ok lis ->
            Ok <|
                M.List
                    { type_ = type_
                    , indentLength = 3
                    , delimiter = delimiter
                    , isLoose = False
                    }
                    lis


blockToMarkdown : BlockNode -> Result String Block
blockToMarkdown node =
    let
        parameters =
            elementParametersFromBlockNode node

        children =
            childNodes node
    in
    case nameFromElementParameters parameters of
        "paragraph" ->
            Result.map (M.Paragraph "") (inlineChildrenToMarkdown children)

        "blockquote" ->
            Result.map M.BlockQuote (blockChildrenToMarkdown children)

        "horizontal_rule" ->
            Ok M.ThematicBreak

        "heading" ->
            headingToMarkdown parameters children

        "code_block" ->
            codeBlockToMarkdown children

        "unordered_list" ->
            listToMarkdown M.Unordered parameters children

        "ordered_list" ->
            listToMarkdown (M.Ordered 1) parameters children

        name ->
            Err ("Unexpected element: " ++ name)


markdownToString : List Block -> Result String String
markdownToString =
    blockMarkdownChildrenToString


escapeForMarkdown : String -> String
escapeForMarkdown s =
    s


inlineMarkdownToString : Inline -> Result String String
inlineMarkdownToString inline =
    case inline of
        MI.Text s ->
            Ok <| escapeForMarkdown s

        MI.HardLineBreak ->
            Ok "  \n"

        MI.CodeInline s ->
            Ok <| "`" ++ s ++ "`"

        MI.Link href title children ->
            Result.map
                (\c ->
                    let
                        t =
                            Maybe.withDefault "" <| Maybe.map (\m -> " \"" ++ m ++ "\"") title
                    in
                    "[" ++ c ++ "](" ++ href ++ t ++ ")"
                )
                (inlineMarkdownChildrenToString children)

        MI.Image url alt children ->
            Result.map
                (\c ->
                    let
                        a =
                            Maybe.withDefault "" <| Maybe.map (\m -> " \"" ++ m ++ "\"") alt
                    in
                    "![" ++ c ++ "](" ++ url ++ a ++ ")"
                )
                (inlineMarkdownChildrenToString children)

        MI.Emphasis length children ->
            let
                e =
                    String.repeat length "*"
            in
            Result.map (\c -> e ++ c ++ e) (inlineMarkdownChildrenToString children)

        MI.HtmlInline tag attributes children ->
            Err "Html inline is not implemented."

        MI.Custom i children ->
            Err "Not implemented"


inlineMarkdownChildrenToString : List Inline -> Result String String
inlineMarkdownChildrenToString inlines =
    Result.map (String.join "") <|
        unwrapAndFilterChildNodes <|
            List.map inlineMarkdownToString inlines


blockMarkdownChildrenToString : List Block -> Result String String
blockMarkdownChildrenToString blocks =
    Result.map (String.join "\n") <|
        unwrapAndFilterChildNodes (List.map markdownBlockToString blocks)


indentEverythingButFirstLine : Int -> String -> String
indentEverythingButFirstLine n s =
    String.join "\n" <|
        List.indexedMap
            (\i x ->
                if i == 0 then
                    x

                else
                    String.repeat n " " ++ x
            )
            (String.split "\n" s)


listMarkdownToString : M.ListBlock -> List (List Block) -> Result String String
listMarkdownToString listBlock listItems =
    Result.map
        (\children ->
            String.join "\n"
                (List.indexedMap
                    (\i z ->
                        let
                            prefix =
                                case listBlock.type_ of
                                    M.Unordered ->
                                        listBlock.delimiter ++ " "

                                    M.Ordered startIndex ->
                                        String.fromInt (startIndex + i) ++ listBlock.delimiter ++ " "
                        in
                        prefix ++ indentEverythingButFirstLine (String.length prefix) z
                    )
                    children
                )
        )
        (unwrapAndFilterChildNodes <|
            List.map blockMarkdownChildrenToString listItems
        )


markdownCodeBlockToString : M.CodeBlock -> String -> Result String String
markdownCodeBlockToString cb s =
    case cb of
        M.Fenced _ fence ->
            let
                delimeter =
                    String.repeat fence.fenceLength fence.fenceChar
            in
            Ok <|
                (delimeter ++ "\n")
                    ++ String.join "\n" (List.map (\v -> String.repeat fence.indentLength " " ++ v) (String.split "\n" s))
                    ++ ("\n" ++ delimeter)

        M.Indented ->
            Ok <| String.join "\n" <| List.map (\v -> "    " ++ v) (String.split "\n" s)


markdownBlockToString : Block -> Result String String
markdownBlockToString block =
    case block of
        M.BlankLine s ->
            Ok <| s

        M.ThematicBreak ->
            Ok <| "---"

        M.Heading s i children ->
            Result.map
                (\x -> String.repeat i "#" ++ " " ++ x)
                (inlineMarkdownChildrenToString children)

        M.CodeBlock cb s ->
            markdownCodeBlockToString cb s

        M.Paragraph s children ->
            Result.map (\x -> x)
                (inlineMarkdownChildrenToString children)

        M.BlockQuote children ->
            Result.map
                (\x ->
                    String.join "\n" (List.map (\m -> "> " ++ m) (String.split "\n" x))
                )
                (blockMarkdownChildrenToString children)

        M.List lb listItems ->
            listMarkdownToString lb listItems

        M.PlainInlines children ->
            inlineMarkdownChildrenToString children

        M.Custom custom children ->
            Err "Custom element are not implemented"


markdownToBlock : List Block -> Result String BlockNode
markdownToBlock md =
    Result.map
        (\children ->
            blockNode
                (elementParameters doc [] Set.empty)
                children
        )
        (markdownBlockListToBlockChildNodes md)


markdownBlockListToBlockChildNodes : List Block -> Result String ChildNodes
markdownBlockListToBlockChildNodes blocks =
    Result.map
        (\items -> blockArray (Array.fromList items))
        (markdownBlockListToBlockLeaves blocks)


markdownBlockListToBlockLeaves : List Block -> Result String (List BlockNode)
markdownBlockListToBlockLeaves blocks =
    unwrapAndFilterChildNodes (List.map markdownBlockToEditorBlock blocks)


markdownInlineListToInlineChildNodes : List Inline -> Result String ChildNodes
markdownInlineListToInlineChildNodes inlines =
    Result.map
        (\items -> inlineLeafArray (Array.fromList items))
        (markdownInlineListToInlineLeaves [] inlines)


markdownInlineListToInlineLeaves : List Mark -> List Inline -> Result String (List InlineLeaf)
markdownInlineListToInlineLeaves marks inlines =
    Result.map
        (\items -> List.concatMap identity items)
        (unwrapAndFilterChildNodes (List.map (markdownInlineToInlineLeaves marks) inlines))


markdownInlineToInlineLeaves : List Mark -> Inline -> Result String (List InlineLeaf)
markdownInlineToInlineLeaves marks inline =
    case inline of
        MI.Text s ->
            Ok <|
                [ textLeaf s (Mark.sort markdownMarkOrder marks) ]

        MI.HardLineBreak ->
            Ok <|
                [ inlineLeaf (elementParameters hardBreak [] Set.empty)
                    []
                ]

        MI.CodeInline s ->
            let
                codeMark =
                    Mark.mark code []
            in
            Ok <| [ textLeaf s (Mark.sort markdownMarkOrder (codeMark :: marks)) ]

        MI.Link href title children ->
            let
                linkMark =
                    Mark.mark link
                        (List.filterMap identity
                            [ Just <| StringAttribute "href" href
                            , Maybe.map (\t -> StringAttribute "title" t) title
                            ]
                        )
            in
            markdownInlineListToInlineLeaves (linkMark :: marks) children

        MI.Image src alt children ->
            let
                inlineImage =
                    inlineLeaf
                        (elementParameters image
                            (List.filterMap identity
                                [ Just <| StringAttribute "src" src
                                , Maybe.map (\t -> StringAttribute "alt" t) alt
                                ]
                            )
                            Set.empty
                        )
                        (Mark.sort markdownMarkOrder marks)
            in
            Ok <| [ inlineImage ]

        MI.Emphasis i children ->
            let
                emphasis =
                    Mark.mark bold [ IntegerAttribute "delimiterLength" i ]
            in
            markdownInlineListToInlineLeaves (emphasis :: marks) children

        MI.HtmlInline _ _ _ ->
            Err "Not implemented"

        MI.Custom _ _ ->
            Err "Not implemented"


markdownCodeBlockToEditorBlock : M.CodeBlock -> String -> Result String BlockNode
markdownCodeBlockToEditorBlock cb s =
    let
        attributes =
            case cb of
                M.Indented ->
                    [ StringAttribute "type" "indented" ]

                M.Fenced b f ->
                    List.filterMap identity
                        [ Just <| BoolAttribute "open" b
                        , Just <| StringAttribute "type" "fenced"
                        , Just <| IntegerAttribute "indentLength" f.indentLength
                        , Just <| IntegerAttribute "fenceLength" f.fenceLength
                        , Maybe.map (\m -> StringAttribute "language" m) f.language
                        ]
    in
    Ok <|
        blockNode
            (elementParameters codeBlock attributes Set.empty)
            (inlineLeafArray <| Array.fromList [ textLeafWithText s ])


markdownListToEditorBlock : M.ListBlock -> List (List Block) -> Result String BlockNode
markdownListToEditorBlock lb children =
    let
        ( node, typeAttributes ) =
            case lb.type_ of
                M.Ordered i ->
                    ( orderedList, [ IntegerAttribute "startIndex" i ] )

                M.Unordered ->
                    ( unorderedList, [] )

        attributes =
            [ IntegerAttribute "indentLength" lb.indentLength
            , StringAttribute "delimiter" lb.delimiter
            ]
                ++ typeAttributes
    in
    Result.map
        (\listItems ->
            blockNode
                (elementParameters node attributes Set.empty)
                (blockArray
                    (Array.fromList
                        (List.map
                            (\cn ->
                                blockNode
                                    (elementParameters listItem [] Set.empty)
                                    cn
                            )
                            listItems
                        )
                    )
                )
        )
        (unwrapAndFilterChildNodes
            (List.map
                (\x -> markdownBlockListToBlockChildNodes x)
                children
            )
        )


markdownInlineToParagraphBlock : List Inline -> Result String BlockNode
markdownInlineToParagraphBlock children =
    Result.map
        (\c ->
            blockNode
                (elementParameters paragraph [] Set.empty)
                c
        )
        (markdownInlineListToInlineChildNodes children)


markdownBlockToEditorBlock : Block -> Result String BlockNode
markdownBlockToEditorBlock block =
    case block of
        M.BlankLine s ->
            Ok <|
                blockNode
                    (elementParameters paragraph [] Set.empty)
                    (inlineLeafArray <| Array.fromList [ textLeafWithText s ])

        M.ThematicBreak ->
            Ok <|
                blockNode
                    (elementParameters horizontalRule [] Set.empty)
                    Leaf

        M.Heading s i children ->
            Result.map
                (\c ->
                    blockNode
                        (elementParameters
                            heading
                            [ IntegerAttribute "level" i ]
                            Set.empty
                        )
                        c
                )
                (markdownInlineListToInlineChildNodes children)

        M.CodeBlock cb s ->
            markdownCodeBlockToEditorBlock cb s

        M.Paragraph _ children ->
            markdownInlineToParagraphBlock children

        M.BlockQuote children ->
            Result.map
                (\c ->
                    blockNode
                        (elementParameters blockquote [] Set.empty)
                        c
                )
                (markdownBlockListToBlockChildNodes children)

        M.List lb listItems ->
            markdownListToEditorBlock lb listItems

        M.PlainInlines children ->
            markdownInlineToParagraphBlock children

        M.Custom _ _ ->
            Err "Custom elements are not implemented"
