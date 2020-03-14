module RichTextEditor.MarkdownSpec exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.Annotation exposing (selectableAnnotation)
import RichTextEditor.Model.Attribute exposing (Attribute(..), findIntegerAttribute, findStringAttribute)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark as Mark exposing (mark)
import RichTextEditor.Model.Node exposing (attributesFromElementParameters, elementParameters)
import RichTextEditor.Model.Spec exposing (ElementToHtml, HtmlToElement, HtmlToMark, MarkDefinition, MarkToHtml, NodeDefinition, Spec, blockLeafContentType, blockNodeContentType, emptySpec, inlineLeafContentType, markDefinition, nodeDefinition, textBlockContentType, withMarkDefinitions, withNodeDefinitions)
import RichTextEditor.Spec exposing (defaultElementToHtml, defaultHtmlToElement, defaultHtmlToMark)
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
htmlToDoc definition node =
    case node of
        ElementNode name attrs children ->
            if name == "div" && attrs == [ ( "data-rte-doc", "true" ) ] then
                Just <| ( elementParameters definition [] Set.empty, children )

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
htmlToParagraph definition node =
    case node of
        ElementNode name _ children ->
            if name == "p" then
                Just <| ( elementParameters definition [] Set.empty, children )

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
    defaultHtmlToElement "blockquote"


horizontalRule : NodeDefinition
horizontalRule =
    nodeDefinition "horizontal_rule" "block" blockLeafContentType horizontalRuleToHtml htmlToHorizontalRule


horizontalRuleToHtml : ElementToHtml
horizontalRuleToHtml =
    defaultElementToHtml "hr"


htmlToHorizontalRule : HtmlToElement
htmlToHorizontalRule def node =
    case node of
        ElementNode name _ _ ->
            if name == "hr" then
                Just ( elementParameters def [] <| Set.fromList [ selectableAnnotation ], Array.empty )

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
            Maybe.withDefault 1 <| findIntegerAttribute "level" (attributesFromElementParameters parameters)
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
                        ( elementParameters def
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
        (textBlockContentType [ "text", "hard_break" ])
        codeBlockToHtmlNode
        htmlNodeToCodeBlock


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
                                Just ( elementParameters def [] Set.empty, childChildren )

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
                [ ( "src", Just <| Maybe.withDefault "" (findStringAttribute "src" (attributesFromElementParameters parameters)) )
                , ( "alt", findStringAttribute "alt" (attributesFromElementParameters parameters) )
                , ( "title", findStringAttribute "title" (attributesFromElementParameters parameters) )
                ]
    in
    ElementNode "img"
        attributes
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
                        ( elementParameters
                            def
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
    defaultHtmlToElement "ol"


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
    defaultHtmlToElement "ul"


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
    defaultHtmlToElement "li"



-- Mark definitions


link : MarkDefinition
link =
    markDefinition "link" linkToHtmlNode htmlNodeToLink


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


bold : MarkDefinition
bold =
    markDefinition "bold" boldToHtmlNode htmlNodeToBold


boldToHtmlNode : MarkToHtml
boldToHtmlNode _ children =
    ElementNode "b" [] children


htmlNodeToBold : HtmlToMark
htmlNodeToBold =
    defaultHtmlToMark "b"


italic : MarkDefinition
italic =
    markDefinition "italic" italicToHtmlNode htmlNodeToItalic


italicToHtmlNode : MarkToHtml
italicToHtmlNode _ children =
    ElementNode "i" [] children


htmlNodeToItalic : HtmlToMark
htmlNodeToItalic =
    defaultHtmlToMark "i"


code : MarkDefinition
code =
    markDefinition "code" codeToHtmlNode htmlNodeToCode


codeToHtmlNode : MarkToHtml
codeToHtmlNode _ children =
    ElementNode "code" [] children


htmlNodeToCode : HtmlToMark
htmlNodeToCode =
    defaultHtmlToMark "code"


spec : Spec
spec =
    emptySpec
        |> withNodeDefinitions
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
