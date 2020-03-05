module BasicSpecs exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Model exposing (ContentType(..), EditorAttribute(..), ElementParameters, ElementToHtml, HtmlNode(..), HtmlToElement, Mark, NodeDefinition, Spec, blockLeafContentType, blockNodeContentType, elementParameters, findIntegerAttribute, findStringAttribute, inlineLeafNodeType, nodeDefinition, selectableAnnotation, textBlockContentType)
import Rte.Spec exposing (defaultElementToHtml, defaultHtmlToElement, defaultHtmlToMark)
import Set


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
boldToHtmlNode mark children =
    ElementNode "b" [] children


italicToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
italicToHtmlNode mark children =
    ElementNode "i" [] children


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


blockquote =
    nodeDefinition "blockquote" "block" (blockNodeContentType [ "block" ]) blockquoteToHtml htmlToBlockquote


blockquoteToHtml : ElementToHtml
blockquoteToHtml =
    defaultElementToHtml "blockquote"


htmlToBlockquote : HtmlToElement
htmlToBlockquote =
    defaultHtmlToElement "blockquote" "blockquote"


horizontalRule =
    nodeDefinition "horizontal_rule" "block" blockLeafContentType blockquoteToHtml htmlToBlockquote


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


codeBlock =
    nodeDefinition
        "code_block"
        "block"
        (textBlockContentType [ "text" ])
        codeBlockToHtmlNode
        htmlNodeToCodeBlock


codeBlockToHtmlNode : ElementToHtml
codeBlockToHtmlNode parameters children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


htmlNodeToCodeBlock : HtmlToElement
htmlNodeToCodeBlock node =
    case node of
        ElementNode name attrs children ->
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


image =
    nodeDefinition "image" "inline" inlineLeafNodeType imageToHtmlNode htmlNodeToImage


imageToHtmlNode : ElementToHtml
imageToHtmlNode parameters _ =
    let
        attributes =
            List.filterMap
                (\( p, v ) ->
                    case v of
                        Nothing ->
                            Nothing

                        Just tv ->
                            Just ( p, tv )
                )
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
        ]
    , marks =
        [ { name = "bold", toHtmlNode = boldToHtmlNode, fromHtmlNode = defaultHtmlToMark "b" "bold" }
        , { name = "italic", toHtmlNode = italicToHtmlNode, fromHtmlNode = defaultHtmlToMark "i" "italic" }
        ]
    }
