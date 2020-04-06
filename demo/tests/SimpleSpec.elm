module SimpleSpec exposing (..)

import Array exposing (Array)
import RichText.Config.ElementDefinition
    exposing
        ( ElementDefinition
        , blockNode
        , defaultElementToHtml
        , defaultHtmlToElement
        , elementDefinition
        , inlineLeaf
        , textBlock
        )
import RichText.Config.MarkDefinition exposing (defaultHtmlToMark, markDefinition)
import RichText.Config.Spec
    exposing
        ( Spec
        , emptySpec
        , withElementDefinitions
        , withMarkDefinitions
        )
import RichText.Model.Element exposing (Element, element)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Mark exposing (Mark)


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


htmlNodeToCrazyBlock : ElementDefinition -> HtmlNode -> Maybe ( Element, Array HtmlNode )
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
                                Just ( element def [], c )

                            _ ->
                                Nothing

        TextNode _ ->
            Nothing


htmlNodeToCodeBlock : ElementDefinition -> HtmlNode -> Maybe ( Element, Array HtmlNode )
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


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
boldToHtmlNode _ children =
    ElementNode "b" [] children


italicToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
italicToHtmlNode _ children =
    ElementNode "i" [] children


codeBlock =
    elementDefinition
        { name = "code_block"
        , group = "block"
        , contentType = blockNode []
        , toHtmlNode = codeBlockToHtmlNode
        , fromHtmlNode = htmlNodeToCodeBlock
        , selectable = False
        }


crazyBlock =
    elementDefinition
        { name = "crazy_block"
        , group = "block"
        , contentType = blockNode []
        , toHtmlNode = crazyBlockToHtmlNode
        , fromHtmlNode = htmlNodeToCrazyBlock
        , selectable = False
        }


paragraph =
    elementDefinition
        { name = "paragraph"
        , group = "block"
        , contentType = textBlock { allowedGroups = [ "inline" ], allowedMarks = [] }
        , toHtmlNode = defaultElementToHtml "p"
        , fromHtmlNode = defaultHtmlToElement "p"
        , selectable = False
        }


image =
    elementDefinition
        { name = "image"
        , group = "inline"
        , contentType = inlineLeaf
        , toHtmlNode = defaultElementToHtml "img"
        , fromHtmlNode = defaultHtmlToElement "img"
        , selectable = False
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
        |> withElementDefinitions
            [ codeBlock
            , crazyBlock
            , paragraph
            , image
            ]
        |> withMarkDefinitions
            [ bold
            , italic
            ]
