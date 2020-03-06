module BasicSpecs exposing (..)

import Array exposing (Array)
import Rte.Model exposing (ContentType(..), EditorAttribute(..), ElementParameters, ElementToHtml, HtmlNode(..), HtmlToElement, HtmlToMark, Mark, MarkDefinition, MarkToHtml, NodeDefinition, Spec, blockLeafContentType, blockNodeContentType, elementParameters, findIntegerAttribute, findStringAttribute, inlineLeafContentType, mark, markDefinition, nodeDefinition, selectableAnnotation, textBlockContentType)
import Rte.Spec exposing (defaultElementToHtml, defaultHtmlToElement, defaultHtmlToMark)
import Set


doc : NodeDefinition
doc =
    nodeDefinition "doc" "root" (blockNodeContentType [ "block" ]) docToHtml htmlToDoc


docToHtml : ElementToHtml
docToHtml _ children =
    ElementNode "div"
        [ ( "data-rte-doc", "true" ) ]
        children


htmlToDoc : HtmlToElement
htmlToDoc node =
    case node of
        ElementNode name attrs children ->
            if name == "div" && attrs == [ ( "data-rte-doc", "true" ) ] then
                Just <| ( elementParameters "doc" [] Set.empty, children )

            else
                Nothing

        _ ->
            Nothing


paragraph : NodeDefinition
paragraph =
    nodeDefinition "paragraph" "block" (textBlockContentType [ "inline" ]) paragraphToHtml htmlToParagraph


paragraphToHtml : ElementToHtml
paragraphToHtml _ children =
    ElementNode "p" [] children


htmlToParagraph : HtmlToElement
htmlToParagraph node =
    case node of
        ElementNode name _ children ->
            if name == "p" then
                Just <| ( elementParameters "p" [] Set.empty, children )

            else
                Nothing

        _ ->
            Nothing


blockquote : NodeDefinition
blockquote =
    nodeDefinition "blockquote" "block" (blockNodeContentType [ "block" ]) blockquoteToHtml htmlToBlockquote


blockquoteToHtml : ElementToHtml
blockquoteToHtml =
    defaultElementToHtml "blockquote"


htmlToBlockquote : HtmlToElement
htmlToBlockquote =
    defaultHtmlToElement "blockquote" "blockquote"


horizontalRule : NodeDefinition
horizontalRule =
    nodeDefinition "horizontal_rule" "block" blockLeafContentType horizontalRuleToHtml htmlToHorizontalRule


horizontalRuleToHtml : ElementToHtml
horizontalRuleToHtml =
    defaultElementToHtml "hr"


htmlToHorizontalRule : HtmlToElement
htmlToHorizontalRule node =
    case node of
        ElementNode name _ _ ->
            if name == "hr" then
                Just ( elementParameters "horizontal_rule" [] <| Set.fromList [ selectableAnnotation ], Array.empty )

            else
                Nothing

        _ ->
            Nothing


heading : NodeDefinition
heading =
    nodeDefinition "heading" "block" (textBlockContentType [ "inline" ]) headingToHtml htmlToHeading


headingToHtml : ElementToHtml
headingToHtml parameters children =
    let
        level =
            Maybe.withDefault 1 <| findIntegerAttribute "level" parameters.attributes
    in
    ElementNode ("h" ++ String.fromInt level) [] children


htmlToHeading : HtmlToElement
htmlToHeading node =
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
                        ( elementParameters "heading"
                            [ IntegerAttribute "level" level ]
                            Set.empty
                        , children
                        )

        _ ->
            Nothing


codeBlock : NodeDefinition
codeBlock =
    nodeDefinition
        "code_block"
        "block"
        (textBlockContentType [ "text" ])
        codeBlockToHtmlNode
        htmlNodeToCodeBlock


codeBlockToHtmlNode : ElementToHtml
codeBlockToHtmlNode _ children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


htmlNodeToCodeBlock : HtmlToElement
htmlNodeToCodeBlock node =
    case node of
        ElementNode name _ children ->
            if name == "pre" && Array.length children == 1 then
                case Array.get 0 children of
                    Nothing ->
                        Nothing

                    Just n ->
                        case n of
                            ElementNode _ _ childChildren ->
                                Just ( elementParameters "code_block" [] Set.empty, childChildren )

                            _ ->
                                Nothing

            else
                Nothing

        _ ->
            Nothing


image : NodeDefinition
image =
    nodeDefinition "image" "inline" inlineLeafContentType imageToHtmlNode htmlNodeToImage


imageToHtmlNode : ElementToHtml
imageToHtmlNode parameters _ =
    let
        attributes =
            filterAttributesToHtml
                [ ( "src", Just <| Maybe.withDefault "" (findStringAttribute "src" parameters.attributes) )
                , ( "alt", findStringAttribute "alt" parameters.attributes )
                , ( "title", findStringAttribute "title" parameters.attributes )
                ]
    in
    ElementNode "img"
        attributes
        Array.empty


htmlNodeToImage : HtmlToElement
htmlNodeToImage node =
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
                        ( elementParameters
                            "img"
                            elementNodeAttributes
                          <|
                            Set.fromList [ selectableAnnotation ]
                        , Array.empty
                        )

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


hardBreak : NodeDefinition
hardBreak =
    nodeDefinition "hard_break" "inline" inlineLeafContentType hardBreakToHtml htmlToHardBreak


hardBreakToHtml : ElementToHtml
hardBreakToHtml =
    defaultElementToHtml "br"


htmlToHardBreak : HtmlToElement
htmlToHardBreak =
    defaultHtmlToElement "br" "hard_break"


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



--- List node definitions


orderedList : NodeDefinition
orderedList =
    nodeDefinition
        "ordered_list"
        "block"
        (blockNodeContentType [ "list_item" ])
        orderedListToHtml
        htmlToOrderedList


orderedListToHtml : ElementToHtml
orderedListToHtml _ children =
    ElementNode "ol" [] children


htmlToOrderedList : HtmlToElement
htmlToOrderedList =
    defaultHtmlToElement "ol" "ordered_list"


unorderedList : NodeDefinition
unorderedList =
    nodeDefinition
        "unordered_list"
        "block"
        (blockNodeContentType [ "list_item" ])
        unorderedListToHtml
        htmlToUnorderedList


unorderedListToHtml : ElementToHtml
unorderedListToHtml _ children =
    ElementNode "ul" [] children


htmlToUnorderedList : HtmlToElement
htmlToUnorderedList =
    defaultHtmlToElement "ul" "unordered_list"


listItem : NodeDefinition
listItem =
    nodeDefinition
        "list_item"
        "list_item"
        (blockNodeContentType [ "block" ])
        listItemToHtml
        htmlToListItem


listItemToHtml : ElementToHtml
listItemToHtml _ children =
    ElementNode "li" [] children


htmlToListItem : HtmlToElement
htmlToListItem =
    defaultHtmlToElement "li" "list_item"



-- Mark definitions


link : MarkDefinition
link =
    markDefinition "link" linkToHtmlNode htmlNodeToLink


linkToHtmlNode : MarkToHtml
linkToHtmlNode mark children =
    let
        attributes =
            filterAttributesToHtml
                [ ( "href", Just <| Maybe.withDefault "" (findStringAttribute "href" mark.attributes) )
                , ( "title", findStringAttribute "title" mark.attributes )
                ]
    in
    ElementNode "a"
        attributes
        children


htmlNodeToLink : HtmlToMark
htmlNodeToLink node =
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
                            "link"
                            elementNodeAttributes
                        , children
                        )

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


bold : MarkDefinition
bold =
    markDefinition "bold" boldToHtmlNode htmlNodeToBold


boldToHtmlNode : MarkToHtml
boldToHtmlNode _ children =
    ElementNode "b" [] children


htmlNodeToBold : HtmlToMark
htmlNodeToBold =
    defaultHtmlToMark "b" "bold"


italic : MarkDefinition
italic =
    markDefinition "italic" italicToHtmlNode htmlNodeToItalic


italicToHtmlNode : MarkToHtml
italicToHtmlNode _ children =
    ElementNode "i" [] children


htmlNodeToItalic : HtmlToMark
htmlNodeToItalic =
    defaultHtmlToMark "i" "italic"


code : MarkDefinition
code =
    markDefinition "code" codeToHtmlNode htmlNodeToCode


codeToHtmlNode : MarkToHtml
codeToHtmlNode _ children =
    ElementNode "code" [] children


htmlNodeToCode : HtmlToMark
htmlNodeToCode =
    defaultHtmlToMark "code" "code"


simpleSpec : Spec
simpleSpec =
    { nodes =
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
    , marks =
        [ link
        , bold
        , italic
        , code
        ]
    }
