module BasicSpecs exposing (..)

import Rte.Model exposing (ElementParameters, HtmlNode(..), Mark, Spec)
import Rte.Spec exposing (childNodesPlaceholder)


codeBlockToHtmlNode : ElementParameters -> List HtmlNode -> HtmlNode
codeBlockToHtmlNode parameters children =
    ElementNode "pre"
        []
        [ ElementNode "code" [] children
        ]


crazyBlockToHtmlNode : ElementParameters -> List HtmlNode -> HtmlNode
crazyBlockToHtmlNode parameters children =
    ElementNode "div"
        []
        [ ElementNode "img" [] []
        , ElementNode "div" [] [ ElementNode "hr" [] [] ]
        , ElementNode "div" [] children
        ]


boldToHtmlNode : Mark -> List HtmlNode -> HtmlNode
boldToHtmlNode mark children =
    ElementNode "b" [] children


simpleSpec : Spec
simpleSpec =
    { nodes =
        [ { name = "code_block", toHtmlNode = codeBlockToHtmlNode }
        , { name = "crazy_block", toHtmlNode = crazyBlockToHtmlNode }
        ]
    , marks = [ { name = "bold", toHtmlNode = boldToHtmlNode } ]
    }
