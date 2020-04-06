module RichText.Definitions exposing
    ( markdown
    , doc, blockquote, codeBlock, hardBreak, heading, horizontalRule, image, paragraph
    , listItem, orderedList, unorderedList
    , bold, code, italic, link
    )

{-|


# Markdown spec

@docs markdown


# Definitions

The definitions used in the markdown specification.


## Elements

@docs doc, blockquote, codeBlock, hardBreak, heading, horizontalRule, image, paragraph

@docs listItem, orderedList, unorderedList


## Marks

@docs bold, code, italic, link

-}

import Array exposing (Array)
import RichText.Annotation exposing (selectable)
import RichText.Config.ElementDefinition
    exposing
        ( ElementDefinition
        , ElementToHtml
        , HtmlToElement
        , blockLeaf
        , blockNode
        , defaultElementToHtml
        , defaultHtmlToElement
        , elementDefinition
        , inlineLeaf
        , textBlock
        )
import RichText.Config.MarkDefinition
    exposing
        ( HtmlToMark
        , MarkDefinition
        , MarkToHtml
        , defaultHtmlToMark
        , markDefinition
        )
import RichText.Config.Spec
    exposing
        ( Spec
        , emptySpec
        , withElementDefinitions
        , withMarkDefinitions
        )
import RichText.Model.Attribute exposing (Attribute(..), findIntegerAttribute, findStringAttribute)
import RichText.Model.Element as Element exposing (attributes, element)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Mark as Mark exposing (mark)
import Set


{-| The root element of a document.
-}
doc : ElementDefinition
doc =
    elementDefinition
        { name = "doc"
        , group = "root"
        , contentType = blockNode [ "block" ]
        , toHtmlNode = docToHtml
        , fromHtmlNode = htmlToDoc
        , selectable = False
        }


docToHtml : ElementToHtml
docToHtml _ children =
    ElementNode "div"
        [ ( "data-rte-doc", "true" ) ]
        children


htmlToDoc : HtmlToElement
htmlToDoc definition node =
    case node of
        ElementNode name attrs children ->
            if name == "div" && attrs == [ ( "data-rte-doc", "true" ) ] then
                Just <| ( element definition [], children )

            else
                Nothing

        _ ->
            Nothing


{-| A paragraph element. It can have inline children.
-}
paragraph : ElementDefinition
paragraph =
    elementDefinition
        { name = "paragraph"
        , group = "block"
        , contentType = textBlock { allowedGroups = [ "inline" ], allowedMarks = [] }
        , toHtmlNode = paragraphToHtml
        , fromHtmlNode = htmlToParagraph
        , selectable = False
        }


paragraphToHtml : ElementToHtml
paragraphToHtml _ children =
    ElementNode "p" [] children


htmlToParagraph : HtmlToElement
htmlToParagraph definition node =
    case node of
        ElementNode name _ children ->
            if name == "p" then
                Just <| ( element definition [], children )

            else
                Nothing

        _ ->
            Nothing


{-| A blockquote element. It can have block children.
-}
blockquote : ElementDefinition
blockquote =
    elementDefinition
        { name = "blockquote"
        , group = "block"
        , contentType = blockNode [ "block" ]
        , toHtmlNode = blockquoteToHtml
        , fromHtmlNode = htmlToBlockquote
        , selectable = False
        }


blockquoteToHtml : ElementToHtml
blockquoteToHtml =
    defaultElementToHtml "blockquote"


htmlToBlockquote : HtmlToElement
htmlToBlockquote =
    defaultHtmlToElement "blockquote"


{-| A horizontal rule element. It is a block leaf, e.g. it can have no children.
-}
horizontalRule : ElementDefinition
horizontalRule =
    elementDefinition
        { name = "horizontal_rule"
        , group = "block"
        , contentType = blockLeaf
        , toHtmlNode = horizontalRuleToHtml
        , fromHtmlNode = htmlToHorizontalRule
        , selectable = True
        }


horizontalRuleToHtml : ElementToHtml
horizontalRuleToHtml =
    defaultElementToHtml "hr"


htmlToHorizontalRule : HtmlToElement
htmlToHorizontalRule def node =
    case node of
        ElementNode name _ _ ->
            if name == "hr" then
                Just ( element def [] |> Element.withAnnotations (Set.fromList [ selectable ]), Array.empty )

            else
                Nothing

        _ ->
            Nothing


{-| A heading element. It can have inline children. It supports one integer attribute `level`,
which defaults to 1 if not set.
-}
heading : ElementDefinition
heading =
    elementDefinition
        { name = "heading"
        , group = "block"
        , contentType = textBlock { allowedGroups = [ "inline" ], allowedMarks = [] }
        , toHtmlNode = headingToHtml
        , fromHtmlNode = htmlToHeading
        , selectable = False
        }


headingToHtml : ElementToHtml
headingToHtml parameters children =
    let
        level =
            Maybe.withDefault 1 <| findIntegerAttribute "level" (attributes parameters)
    in
    ElementNode ("h" ++ String.fromInt level) [] children


htmlToHeading : HtmlToElement
htmlToHeading def node =
    case node of
        ElementNode name _ children ->
            let
                maybeLevel =
                    case name of
                        "h1" ->
                            Just 1

                        "h2" ->
                            Just 2

                        "h3" ->
                            Just 3

                        "h4" ->
                            Just 4

                        "h5" ->
                            Just 5

                        "h6" ->
                            Just 6

                        _ ->
                            Nothing
            in
            case maybeLevel of
                Nothing ->
                    Nothing

                Just level ->
                    Just <|
                        ( element def
                            [ IntegerAttribute "level" level ]
                        , children
                        )

        _ ->
            Nothing


{-| A code block element.
-}
codeBlock : ElementDefinition
codeBlock =
    elementDefinition
        { name = "code_block"
        , group = "block"
        , contentType = textBlock { allowedGroups = [ "text" ], allowedMarks = [ "__nothing__" ] }
        , toHtmlNode = codeBlockToHtmlNode
        , fromHtmlNode = htmlNodeToCodeBlock
        , selectable = False
        }


codeBlockToHtmlNode : ElementToHtml
codeBlockToHtmlNode _ children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


htmlNodeToCodeBlock : HtmlToElement
htmlNodeToCodeBlock def node =
    case node of
        ElementNode name _ children ->
            if name == "pre" && Array.length children == 1 then
                case Array.get 0 children of
                    Nothing ->
                        Nothing

                    Just n ->
                        case n of
                            ElementNode _ _ childChildren ->
                                Just ( element def [], childChildren )

                            _ ->
                                Nothing

            else
                Nothing

        _ ->
            Nothing


{-| An inline image. It can have three different string attributes:

  - `src` is the uri of the image to show
  - `alt` is the alt text of the image
  - `title` is the title of the image

-}
image : ElementDefinition
image =
    elementDefinition
        { name = "image"
        , group = "inline"
        , contentType = inlineLeaf
        , toHtmlNode = imageToHtmlNode
        , fromHtmlNode = htmlNodeToImage
        , selectable = True
        }


imageToHtmlNode : ElementToHtml
imageToHtmlNode parameters _ =
    let
        attr =
            filterAttributesToHtml
                [ ( "src", Just <| Maybe.withDefault "" (findStringAttribute "src" (attributes parameters)) )
                , ( "alt", findStringAttribute "alt" (attributes parameters) )
                , ( "title", findStringAttribute "title" (attributes parameters) )
                ]
    in
    ElementNode "img"
        attr
        Array.empty


htmlNodeToImage : HtmlToElement
htmlNodeToImage def node =
    case node of
        ElementNode name attributes _ ->
            if name == "img" then
                let
                    elementNodeAttributes =
                        List.filterMap
                            (\( k, v ) ->
                                case k of
                                    "src" ->
                                        Just <| StringAttribute "src" v

                                    "alt" ->
                                        Just <| StringAttribute "alt" v

                                    "title" ->
                                        Just <| StringAttribute "title" v

                                    _ ->
                                        Nothing
                            )
                            attributes
                in
                if findStringAttribute "src" elementNodeAttributes /= Nothing then
                    Just
                        ( element
                            def
                            elementNodeAttributes
                            |> Element.withAnnotations (Set.singleton selectable)
                        , Array.empty
                        )

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


{-| Hard break is an inline leaf which represents a line break in a document.
-}
hardBreak : ElementDefinition
hardBreak =
    elementDefinition
        { name = "hard_break"
        , group = "inline"
        , contentType = inlineLeaf
        , toHtmlNode = hardBreakToHtml
        , fromHtmlNode = htmlToHardBreak
        , selectable = False
        }


hardBreakToHtml : ElementToHtml
hardBreakToHtml =
    defaultElementToHtml "br"


htmlToHardBreak : HtmlToElement
htmlToHardBreak =
    defaultHtmlToElement "br"


filterAttributesToHtml : List ( String, Maybe String ) -> List ( String, String )
filterAttributesToHtml attrs =
    List.filterMap
        (\( p, v ) ->
            case v of
                Nothing ->
                    Nothing

                Just tv ->
                    Just ( p, tv )
        )
        attrs



--- List element definitions


{-| An ordered list element definition. It can have list item children.
-}
orderedList : ElementDefinition
orderedList =
    elementDefinition
        { name = "ordered_list"
        , group = "block"
        , contentType = blockNode [ "list_item" ]
        , toHtmlNode = orderedListToHtml
        , fromHtmlNode = htmlToOrderedList
        , selectable = False
        }


orderedListToHtml : ElementToHtml
orderedListToHtml _ children =
    ElementNode "ol" [] children


htmlToOrderedList : HtmlToElement
htmlToOrderedList =
    defaultHtmlToElement "ol"


{-| An unordered list element definition. It can have list item children.
-}
unorderedList : ElementDefinition
unorderedList =
    elementDefinition
        { name = "unordered_list"
        , group = "block"
        , contentType = blockNode [ "list_item" ]
        , toHtmlNode = unorderedListToHtml
        , fromHtmlNode = htmlToUnorderedList
        , selectable = False
        }


unorderedListToHtml : ElementToHtml
unorderedListToHtml _ children =
    ElementNode "ul" [] children


htmlToUnorderedList : HtmlToElement
htmlToUnorderedList =
    defaultHtmlToElement "ul"


{-| A list item element definition. It can have block children.
-}
listItem : ElementDefinition
listItem =
    elementDefinition
        { name = "list_item"
        , group = "list_item"
        , contentType = blockNode [ "block" ]
        , toHtmlNode = listItemToHtml
        , fromHtmlNode = htmlToListItem
        , selectable = False
        }


listItemToHtml : ElementToHtml
listItemToHtml _ children =
    ElementNode "li" [] children


htmlToListItem : HtmlToElement
htmlToListItem =
    defaultHtmlToElement "li"



-- Mark definitions


{-| A link mark definition. It can have two different string attributes:

  - `href` is the url to link to
  - `title` is the title of the link

-}
link : MarkDefinition
link =
    markDefinition { name = "link", toHtmlNode = linkToHtmlNode, fromHtmlNode = htmlNodeToLink }


linkToHtmlNode : MarkToHtml
linkToHtmlNode mark children =
    let
        attributes =
            filterAttributesToHtml
                [ ( "href", Just <| Maybe.withDefault "" (findStringAttribute "href" (Mark.attributes mark)) )
                , ( "title", findStringAttribute "title" (Mark.attributes mark) )
                ]
    in
    ElementNode "a"
        attributes
        children


htmlNodeToLink : HtmlToMark
htmlNodeToLink def node =
    case node of
        ElementNode name attributes children ->
            if name == "a" then
                let
                    elementNodeAttributes =
                        List.filterMap
                            (\( k, v ) ->
                                case k of
                                    "href" ->
                                        Just <| StringAttribute "src" v

                                    "title" ->
                                        Just <| StringAttribute "title" v

                                    _ ->
                                        Nothing
                            )
                            attributes
                in
                if findStringAttribute "href" elementNodeAttributes /= Nothing then
                    Just
                        ( mark
                            def
                            elementNodeAttributes
                        , children
                        )

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


{-| A bold mark definition.
-}
bold : MarkDefinition
bold =
    markDefinition { name = "bold", toHtmlNode = boldToHtmlNode, fromHtmlNode = htmlNodeToBold }


boldToHtmlNode : MarkToHtml
boldToHtmlNode _ children =
    ElementNode "b" [] children


htmlNodeToBold : HtmlToMark
htmlNodeToBold =
    defaultHtmlToMark "b"


{-| An italic mark definition.
-}
italic : MarkDefinition
italic =
    markDefinition
        { name = "italic"
        , toHtmlNode = italicToHtmlNode
        , fromHtmlNode = htmlNodeToItalic
        }


italicToHtmlNode : MarkToHtml
italicToHtmlNode _ children =
    ElementNode "i" [] children


htmlNodeToItalic : HtmlToMark
htmlNodeToItalic =
    defaultHtmlToMark "i"


{-| A code mark definition.
-}
code : MarkDefinition
code =
    markDefinition { name = "code", toHtmlNode = codeToHtmlNode, fromHtmlNode = htmlNodeToCode }


codeToHtmlNode : MarkToHtml
codeToHtmlNode _ children =
    ElementNode "code" [] children


htmlNodeToCode : HtmlToMark
htmlNodeToCode =
    defaultHtmlToMark "code"


{-| A spec which is compatible with the CommonMark flavor of markdown.
-}
markdown : Spec
markdown =
    emptySpec
        |> withElementDefinitions
            [ doc
            , paragraph
            , blockquote
            , horizontalRule
            , heading
            , codeBlock
            , image
            , hardBreak
            , unorderedList
            , orderedList
            , listItem
            ]
        |> withMarkDefinitions
            [ link
            , bold
            , italic
            , code
            ]
