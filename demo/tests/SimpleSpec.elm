module SimpleSpec exposing (..)

import Array exposing (Array)
import Rte.Model exposing (ContentType(..), EditorFragment(..), EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, Spec, inlineLeafArray)
import Rte.Spec exposing (defaultElementToHtml, defaultHtmlToElement, defaultHtmlToMark, htmlToElementArray)
import Set


codeBlockToHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
codeBlockToHtmlNode parameters children =
    ElementNode "pre"
        []
        (Array.fromList [ ElementNode "code" [] children ])


crazyBlockToHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
crazyBlockToHtmlNode parameters children =
    ElementNode "div"
        []
    <|
        Array.fromList
            [ ElementNode "img" [] Array.empty
            , ElementNode "div" [] (Array.fromList [ ElementNode "hr" [] Array.empty ])
            , ElementNode "div" [] children
            ]


htmlNodeToCrazyBlock : HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
htmlNodeToCrazyBlock node =
    case node of
        ElementNode name attrs children ->
            if name == "div" && Array.length children /= 3 then
                Nothing

            else
                case Array.get 2 children of
                    Nothing ->
                        Nothing

                    Just n ->
                        case n of
                            ElementNode _ a c ->
                                Just ( { name = "crazy_block", attributes = [], annotations = Set.empty }, c )

                            _ ->
                                Nothing

        TextNode _ ->
            Nothing


htmlNodeToCodeBlock : HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
htmlNodeToCodeBlock node =
    case node of
        ElementNode name attrs children ->
            if name == "pre" && Array.length children == 1 then
                case Array.get 0 children of
                    Nothing ->
                        Nothing

                    Just n ->
                        case n of
                            ElementNode childName childAttrs childChildren ->
                                Just ( { name = "code_block", attributes = [], annotations = Set.empty }, childChildren )

                            _ ->
                                Nothing

            else
                Nothing

        _ ->
            Nothing


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
boldToHtmlNode mark children =
    ElementNode "b" [] children


italicToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
italicToHtmlNode mark children =
    ElementNode "i" [] children


simpleSpec : Spec
simpleSpec =
    { nodes =
        [ { name = "code_block"
          , toHtmlNode = codeBlockToHtmlNode
          , fromHtmlNode = htmlNodeToCodeBlock
          , contentType = BlockNodeType Nothing
          }
        , { name = "crazy_block"
          , toHtmlNode = crazyBlockToHtmlNode
          , fromHtmlNode = htmlNodeToCrazyBlock
          , contentType = BlockNodeType Nothing
          }
        , { name = "paragraph"
          , toHtmlNode = defaultElementToHtml "p"
          , fromHtmlNode = defaultHtmlToElement "p" "paragraph"
          , contentType = TextBlockNodeType Nothing
          }
        , { name = "image"
          , toHtmlNode = defaultElementToHtml "img"
          , fromHtmlNode = defaultHtmlToElement "img" "image"
          , contentType = InlineLeafNodeType
          }
        ]
    , marks =
        [ { name = "bold"
          , toHtmlNode = boldToHtmlNode
          , fromHtmlNode = defaultHtmlToMark "b" "bold"
          }
        , { name = "italic"
          , toHtmlNode = italicToHtmlNode
          , fromHtmlNode = defaultHtmlToMark "i" "italic"
          }
        ]
    }
