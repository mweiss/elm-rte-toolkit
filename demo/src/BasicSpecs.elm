module BasicSpecs exposing (..)

import Array exposing (Array)
import Rte.Model exposing (ElementParameters, HtmlNode(..), Mark, Spec)
import Rte.Spec exposing (childNodesPlaceholder)


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


boldToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
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
