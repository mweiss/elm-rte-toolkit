module Page.Markdown exposing (..)

import Array exposing (Array)
import Controls exposing (EditorMsg(..), Style(..))
import Editor
import Html exposing (Html, a, div, h1, p, text, textarea)
import Html.Attributes exposing (href, title)
import Html.Events
import Links exposing (rteToolkit)
import Markdown.Block as M
import Markdown.Config as M
import Markdown.Inline as MI
import RichText.Definitions as MarkdownSpec
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
        , italic
        , link
        , listItem
        , orderedList
        , paragraph
        , unorderedList
        )
import RichText.Editor as RTE exposing (state)
import RichText.Model.Attribute
    exposing
        ( Attribute(..)
        , findIntegerAttribute
        , findStringAttribute
        )
import RichText.Model.Element as Element exposing (Element, element)
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark as Mark exposing (Mark, MarkOrder, markOrderFromSpec)
import RichText.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , InlineTree(..)
        , block
        , blockChildren
        , childNodes
        , inlineChildren
        , inlineElement
        , markedText
        , plainText
        , toBlockArray
        , toInlineArray
        , toInlineTree
        )
import RichText.Model.State as State exposing (State)
import RichText.Model.Text as Text
import Session exposing (Session)


type alias CustomInline =
    {}


type alias CustomBlock =
    {}


type alias MBlock =
    M.Block CustomBlock CustomInline


type alias MInline =
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


config =
    RTE.config
        { decorations = Editor.decorations
        , commandMap = Editor.commandBindings MarkdownSpec.markdown
        , spec = MarkdownSpec.markdown
        , toMsg = InternalMsg
        }


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
            [ text """This is a markdown example.  In combination with the """
            , Html.code [] [ text "pablohirafuji/elm-markdown" ]
            , text """ package, it coverts to and from CommonMark and the editor's state."""
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
                Html.map EditorMsg (Editor.view config model.editor)

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
    markOrderFromSpec MarkdownSpec.markdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( e, _ ) =
                    Editor.update config editorMsg model.editor
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


{-| We filter out blank lines so we don't render them in the document.
-}
filterBlankLines : List MBlock -> List MBlock
filterBlankLines blocks =
    let
        newBlocks =
            List.filterMap
                (\block ->
                    case block of
                        M.BlankLine _ ->
                            Nothing

                        M.BlockQuote children ->
                            Just <| M.BlockQuote (filterBlankLines children)

                        M.List lb listItems ->
                            Just <| M.List lb (List.map filterBlankLines listItems)

                        _ ->
                            Just block
                )
                blocks
    in
    if List.isEmpty newBlocks then
        blocks

    else
        newBlocks


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
            markdownToBlock (filterBlankLines markdownNodes)
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
    in
    { initialEditor | styles = [ Bold, Italic ] }


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


blockChildrenToMarkdown : Children -> Result String (List MBlock)
blockChildrenToMarkdown cn =
    case cn of
        BlockChildren a ->
            let
                results =
                    List.map blockToMarkdown (Array.toList (toBlockArray a))
            in
            unwrapAndFilterChildNodes results

        InlineChildren _ ->
            Err "Invalid child nodes, received inline, expected block"

        Leaf ->
            Err "Invalid child nodes, received leaf, expected block"


inlineChildrenToMarkdown : Children -> Result String (List MInline)
inlineChildrenToMarkdown cn =
    case cn of
        InlineChildren a ->
            let
                results =
                    List.map (inlineToMarkdown (toInlineArray a)) (Array.toList (toInlineTree a))
            in
            Result.map (List.concatMap identity) (unwrapAndFilterChildNodes results)

        BlockChildren _ ->
            Err "Invalid child nodes, was expected inline, received block"

        Leaf ->
            Err "Invalid child nodes, was expected inline, received leaf"


rootToMarkdown : Block -> Result String (List MBlock)
rootToMarkdown node =
    let
        children =
            childNodes node
    in
    blockChildrenToMarkdown children


imageToMarkdown : Element -> Result String MInline
imageToMarkdown parameters =
    let
        attributes =
            Element.attributes parameters

        alt =
            findStringAttribute "alt" attributes
    in
    case findStringAttribute "src" attributes of
        Nothing ->
            Err "No src attribute found"

        Just src ->
            Ok <| MI.Image src alt []


inlineToMarkdown : Array Inline -> InlineTree -> Result String (List MInline)
inlineToMarkdown leaves tree =
    case tree of
        LeafNode i ->
            case Array.get i leaves of
                Nothing ->
                    Err "Invalid leaf tree"

                Just inlineLeaf ->
                    case inlineLeaf of
                        Text p ->
                            Ok <| [ MI.Text (Text.text p) ]

                        InlineElement il ->
                            let
                                parameters =
                                    InlineElement.element il
                            in
                            case Element.name parameters of
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

                        "italic" ->
                            Ok <| [ MI.Emphasis 1 flattenedChildren ]

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


textFromChildNodes : Children -> String
textFromChildNodes cn =
    case cn of
        InlineChildren il ->
            String.join "" <|
                Array.toList <|
                    Array.map
                        (\l ->
                            case l of
                                Text tl ->
                                    Text.text tl

                                InlineElement p ->
                                    if
                                        Element.name
                                            (InlineElement.element p)
                                            == "hard_break"
                                    then
                                        "\n"

                                    else
                                        ""
                        )
                        (toInlineArray il)

        _ ->
            ""


headingToMarkdown : Element -> Children -> Result String MBlock
headingToMarkdown p cn =
    let
        attributes =
            Element.attributes p

        level =
            Maybe.withDefault 1 (findIntegerAttribute "level" attributes)
    in
    Result.map (M.Heading "" level) (inlineChildrenToMarkdown cn)


codeBlockToMarkdown : Children -> Result String MBlock
codeBlockToMarkdown cn =
    let
        t =
            textFromChildNodes cn
    in
    Ok <| M.CodeBlock M.Indented t


listToMarkdown : M.ListType -> Element -> Children -> Result String MBlock
listToMarkdown type_ parameters cn =
    let
        delimiter =
            Maybe.withDefault "." <|
                findStringAttribute
                    "delimiter"
                    (Element.attributes parameters)

        listItems =
            case cn of
                BlockChildren a ->
                    let
                        children =
                            Array.toList <| toBlockArray a
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


blockToMarkdown : Block -> Result String MBlock
blockToMarkdown node =
    let
        parameters =
            Node.element node

        children =
            childNodes node
    in
    case Element.name parameters of
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


markdownToString : List MBlock -> Result String String
markdownToString =
    blockMarkdownChildrenToString


escapeForMarkdown : String -> String
escapeForMarkdown s =
    s


inlineMarkdownToString : MInline -> Result String String
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

        MI.HtmlInline _ _ _ ->
            Err "Html inline is not implemented."

        MI.Custom _ _ ->
            Err "Custom elements are not implemented"


inlineMarkdownChildrenToString : List MInline -> Result String String
inlineMarkdownChildrenToString inlines =
    Result.map (String.join "") <|
        unwrapAndFilterChildNodes <|
            List.map inlineMarkdownToString inlines


blockMarkdownChildrenToString : List MBlock -> Result String String
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


listMarkdownToString : M.ListBlock -> List (List MBlock) -> Result String String
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


markdownBlockToString : MBlock -> Result String String
markdownBlockToString block =
    case block of
        M.BlankLine s ->
            Ok <| s

        M.ThematicBreak ->
            Ok <| "---"

        M.Heading _ i children ->
            Result.map
                (\x -> String.repeat i "#" ++ " " ++ x)
                (inlineMarkdownChildrenToString children)

        M.CodeBlock cb s ->
            markdownCodeBlockToString cb s

        M.Paragraph _ children ->
            Result.map (\x -> x ++ "\n") <|
                inlineMarkdownChildrenToString children

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

        M.Custom _ _ ->
            Err "Custom element are not implemented"


markdownToBlock : List MBlock -> Result String Block
markdownToBlock md =
    Result.map
        (\children ->
            block
                (element doc [])
                children
        )
        (markdownBlockListToBlockChildNodes md)


markdownBlockListToBlockChildNodes : List MBlock -> Result String Children
markdownBlockListToBlockChildNodes blocks =
    Result.map
        (\items -> blockChildren (Array.fromList items))
        (markdownBlockListToBlockLeaves blocks)


markdownBlockListToBlockLeaves : List MBlock -> Result String (List Block)
markdownBlockListToBlockLeaves blocks =
    unwrapAndFilterChildNodes (List.map markdownBlockToEditorBlock blocks)


markdownInlineListToInlineChildNodes : List MInline -> Result String Children
markdownInlineListToInlineChildNodes inlines =
    Result.map
        (\items -> inlineChildren (Array.fromList items))
        (markdownInlineListToInlineLeaves [] inlines)


markdownInlineListToInlineLeaves : List Mark -> List MInline -> Result String (List Inline)
markdownInlineListToInlineLeaves marks inlines =
    Result.map
        (\items -> List.concatMap identity items)
        (unwrapAndFilterChildNodes (List.map (markdownInlineToInlineLeaves marks) inlines))


markdownInlineToInlineLeaves : List Mark -> MInline -> Result String (List Inline)
markdownInlineToInlineLeaves marks inline =
    case inline of
        MI.Text s ->
            Ok <|
                [ markedText s (Mark.sort markdownMarkOrder marks) ]

        MI.HardLineBreak ->
            Ok <|
                [ inlineElement (element hardBreak []) [] ]

        MI.CodeInline s ->
            let
                codeMark =
                    Mark.mark code []
            in
            Ok <| [ markedText s (Mark.sort markdownMarkOrder (codeMark :: marks)) ]

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

        MI.Image src alt _ ->
            let
                inlineImage =
                    inlineElement
                        (element image
                            (List.filterMap identity
                                [ Just <| StringAttribute "src" src
                                , Maybe.map (\t -> StringAttribute "alt" t) alt
                                ]
                            )
                        )
                        (Mark.sort markdownMarkOrder marks)
            in
            Ok <| [ inlineImage ]

        MI.Emphasis i children ->
            let
                emphasis =
                    case i of
                        1 ->
                            [ Mark.mark italic [] ]

                        2 ->
                            [ Mark.mark bold [] ]

                        3 ->
                            [ Mark.mark bold [], Mark.mark italic [] ]

                        _ ->
                            []
            in
            markdownInlineListToInlineLeaves (emphasis ++ marks) children

        MI.HtmlInline _ _ _ ->
            Err "Not implemented"

        MI.Custom _ _ ->
            Err "Not implemented"


markdownCodeBlockToEditorBlock : M.CodeBlock -> String -> Result String Block
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
        block
            (element codeBlock attributes)
            (inlineChildren <| Array.fromList [ plainText s ])


markdownListToEditorBlock : M.ListBlock -> List (List MBlock) -> Result String Block
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
            block
                (element node attributes)
                (blockChildren
                    (Array.fromList
                        (List.map
                            (\cn ->
                                block
                                    (element listItem [])
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


markdownInlineToParagraphBlock : List MInline -> Result String Block
markdownInlineToParagraphBlock children =
    Result.map
        (\c ->
            block
                (element paragraph [])
                c
        )
        (markdownInlineListToInlineChildNodes children)


markdownBlockToEditorBlock : MBlock -> Result String Block
markdownBlockToEditorBlock mblock =
    case mblock of
        M.BlankLine s ->
            Ok <|
                block
                    (element paragraph [])
                    (inlineChildren <| Array.fromList [ plainText s ])

        M.ThematicBreak ->
            Ok <|
                block
                    (element horizontalRule [])
                    Leaf

        M.Heading _ i children ->
            Result.map
                (\c ->
                    block
                        (element
                            heading
                            [ IntegerAttribute "level" i ]
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
                    block
                        (element blockquote [])
                        c
                )
                (markdownBlockListToBlockChildNodes children)

        M.List lb listItems ->
            markdownListToEditorBlock lb listItems

        M.PlainInlines children ->
            markdownInlineToParagraphBlock children

        M.Custom _ _ ->
            Err "Custom elements are not implemented"
