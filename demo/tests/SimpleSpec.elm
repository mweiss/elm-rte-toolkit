module SimpleSpec exposing (..)

import Array exposing (Array)
import RichTextEditor.Config.MarkDefinition exposing (defaultHtmlToMark, markDefinition)
import RichTextEditor.Config.NodeDefinition
    exposing
        ( NodeDefinition
        , blockNode
        , defaultElementToHtml
        , defaultHtmlToElement
        , inlineLeaf
        , nodeDefinition
        , textBlock
        )
import RichTextEditor.Config.Spec
    exposing
        ( Spec
        , emptySpec
        , withMarkDefinitions
        , withNodeDefinitions
        )
import RichTextEditor.Model.Element exposing (Element, element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark exposing (Mark)
import Set


codeBlockToHtmlNode : Element -> Array HtmlNode -> HtmlNode
codeBlockToHtmlNode _ children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


crazyBlockToHtmlNode : Element -> Array HtmlNode -> HtmlNode
crazyBlockToHtmlNode _ children =
    ElementNode "div"
        []
    <|
        Array.fromList
            [ ElementNode "img" [] Array.empty
            , ElementNode "div" [] (Array.fromList [ ElementNode "hr" [] Array.empty ])
            , ElementNode "div" [] children
            ]


htmlNodeToCrazyBlock : NodeDefinition -> HtmlNode -> Maybe ( Element, Array HtmlNode )
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
                                Just ( element def [] Set.empty, c )

                            _ ->
                                Nothing

        TextNode _ ->
            Nothing


htmlNodeToCodeBlock : NodeDefinition -> HtmlNode -> Maybe ( Element, Array HtmlNode )
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
                                Just ( element def [] Set.empty, childChildren )

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
        { name = "code_block"
        , group = "block"
        , contentType = blockNode []
        , toHtmlNode = codeBlockToHtmlNode
        , fromHtmlNode = htmlNodeToCodeBlock
        }


crazyBlock =
    nodeDefinition
        { name = "crazy_block"
        , group = "block"
        , contentType = blockNode []
        , toHtmlNode = crazyBlockToHtmlNode
        , fromHtmlNode = htmlNodeToCrazyBlock
        }


paragraph =
    nodeDefinition
        { name = "paragraph"
        , group = "block"
        , contentType = textBlock []
        , toHtmlNode = defaultElementToHtml "p"
        , fromHtmlNode = defaultHtmlToElement "p"
        }


image =
    nodeDefinition
        { name = "image"
        , group = "inline"
        , contentType = inlineLeaf
        , toHtmlNode = defaultElementToHtml "img"
        , fromHtmlNode = defaultHtmlToElement "img"
        }


bold =
    markDefinition
        { name = "bold"
        , toHtmlNode = boldToHtmlNode
        , fromHtmlNode = defaultHtmlToMark "b"
        }


italic =
    markDefinition
        { name = "italic"
        , toHtmlNode = italicToHtmlNode
        , fromHtmlNode = defaultHtmlToMark "i"
        }


strikethrough =
    markDefinition
        { name = "strikethrough"
        , toHtmlNode = italicToHtmlNode
        , fromHtmlNode = defaultHtmlToMark "s"
        }


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
