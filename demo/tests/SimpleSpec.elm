module SimpleSpec exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Internal.Spec exposing (NodeDefinition)
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node exposing (ElementParameters, elementParameters)
import RichTextEditor.Model.Spec exposing (Spec, blockNodeContentType, emptySpec, inlineLeafContentType, markDefinition, nodeDefinition, textBlockContentType, withMarkDefinitions, withNodeDefinitions)
import RichTextEditor.Spec exposing (defaultElementToHtml, defaultHtmlToElement, defaultHtmlToMark)
import Set


codeBlockToHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
codeBlockToHtmlNode _ children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


crazyBlockToHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
crazyBlockToHtmlNode _ children =
    ElementNode "div"
        []
    <|
        Array.fromList
            [ ElementNode "img" [] Array.empty
            , ElementNode "div" [] (Array.fromList [ ElementNode "hr" [] Array.empty ])
            , ElementNode "div" [] children
            ]


htmlNodeToCrazyBlock : NodeDefinition -> HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
htmlNodeToCrazyBlock def node =
    case node of
        ElementNode name _ children ->
            if name == "div" && Array.length children /= 3 then
                Nothing

            else
                case Array.get 2 children of
                    Nothing ->
                        Nothing

                    Just n ->
                        case n of
                            ElementNode _ _ c ->
                                Just ( elementParameters def [] Set.empty, c )

                            _ ->
                                Nothing

        TextNode _ ->
            Nothing


htmlNodeToCodeBlock : NodeDefinition -> HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
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


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
boldToHtmlNode _ children =
    ElementNode "b" [] children


italicToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
italicToHtmlNode _ children =
    ElementNode "i" [] children


codeBlock =
    nodeDefinition
        "code_block"
        "block"
        (blockNodeContentType [])
        codeBlockToHtmlNode
        htmlNodeToCodeBlock


crazyBlock =
    nodeDefinition
        "crazy_block"
        "block"
        (blockNodeContentType [])
        crazyBlockToHtmlNode
        htmlNodeToCrazyBlock


paragraph =
    nodeDefinition
        "paragraph"
        "block"
        (textBlockContentType [])
        (defaultElementToHtml "p")
        (defaultHtmlToElement "p")


image =
    nodeDefinition
        "image"
        "inline"
        inlineLeafContentType
        (defaultElementToHtml "img")
        (defaultHtmlToElement "img")


bold =
    markDefinition "bold" boldToHtmlNode (defaultHtmlToMark "b")


italic =
    markDefinition "italic" italicToHtmlNode (defaultHtmlToMark "i")


strikethrough =
    markDefinition "strikethrough" italicToHtmlNode (defaultHtmlToMark "s")


simpleSpec : Spec
simpleSpec =
    emptySpec
        |> withNodeDefinitions
            [ codeBlock
            , crazyBlock
            , paragraph
            , image
            ]
        |> withMarkDefinitions
            [ bold
            , italic
            ]
