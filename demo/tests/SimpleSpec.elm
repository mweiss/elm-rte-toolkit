module SimpleSpec exposing (..)

import Array exposing (Array)
import RichTextEditor.Internal.Model
    exposing
        ( ContentType(..)
        , EditorFragment(..)
        , EditorInlineLeaf(..)
        , ElementParameters
        , HtmlNode(..)
        , Mark
        , Spec
        , blockNodeContentType
        , elementParameters
        , inlineLeafContentType
        , markDefinition
        , nodeDefinition
        , textBlockContentType
        )
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


htmlNodeToCrazyBlock : HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
htmlNodeToCrazyBlock node =
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
                                Just ( elementParameters "crazy_block" [] Set.empty, c )

                            _ ->
                                Nothing

        TextNode _ ->
            Nothing


htmlNodeToCodeBlock : HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )
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


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
boldToHtmlNode _ children =
    ElementNode "b" [] children


italicToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
italicToHtmlNode _ children =
    ElementNode "i" [] children


simpleSpec : Spec
simpleSpec =
    { nodes =
        [ nodeDefinition
            "code_block"
            "block"
            (blockNodeContentType [])
            codeBlockToHtmlNode
            htmlNodeToCodeBlock
        , nodeDefinition
            "crazy_block"
            "block"
            (blockNodeContentType [])
            crazyBlockToHtmlNode
            htmlNodeToCrazyBlock
        , nodeDefinition
            "paragraph"
            "block"
            (textBlockContentType [])
            (defaultElementToHtml "p")
            (defaultHtmlToElement "p" "paragraph")
        , nodeDefinition
            "image"
            "inline"
            inlineLeafContentType
            (defaultElementToHtml "img")
            (defaultHtmlToElement "img" "image")
        ]
    , marks =
        [ markDefinition "bold" boldToHtmlNode (defaultHtmlToMark "b" "bold")
        , markDefinition "italic" italicToHtmlNode (defaultHtmlToMark "i" "italic")
        ]
    }
