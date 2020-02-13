module TestDomNode exposing (..)

import Array
import Expect
import Rte.DomNode exposing (domElementNodeType, domTextNodeType, findTextChanges)
import Rte.Model exposing (DomNode(..), HtmlNode(..))
import Test exposing (Test, describe, test)


pHtmlNode =
    ElementNode "p" [] <| Array.fromList [ TextNode "sample" ]


pHtmlNodeDifferentText =
    ElementNode "p" [] <| Array.fromList [ TextNode "sample2" ]


pWithImgHtmlNode =
    ElementNode "p" [] <| Array.fromList [ ElementNode "img" [] Array.empty, TextNode "sample" ]


divHtmlNode =
    ElementNode "div" [] <| Array.fromList [ TextNode "sample" ]


pWithImgDomNode =
    DomNode
        { nodeValue = Nothing
        , nodeType = domElementNodeType
        , childNodes =
            Just <|
                Array.fromList
                    [ DomNode { nodeValue = Nothing, nodeType = domElementNodeType, childNodes = Just Array.empty, tagName = Just "IMG" }
                    , DomNode { nodeValue = Just "sample", nodeType = domTextNodeType, childNodes = Nothing, tagName = Nothing }
                    ]
        , tagName = Just "P"
        }


pDomNode =
    DomNode
        { nodeValue = Nothing
        , nodeType = domElementNodeType
        , childNodes =
            Just <|
                Array.fromList
                    [ DomNode { nodeValue = Just "sample", nodeType = domTextNodeType, childNodes = Nothing, tagName = Nothing }
                    ]
        , tagName = Just "P"
        }


testFindTextChanges : Test
testFindTextChanges =
    describe "Tests the function which finds any text changes between the HtmlNode representation and the actual DOM representation"
        [ test "Test the same structure returns the no text change" <|
            \_ ->
                Expect.equal (Just []) (findTextChanges pHtmlNode pDomNode)
        , test "Different type of node results in Nothing" <|
            \_ ->
                Expect.equal Nothing (findTextChanges divHtmlNode pDomNode)
        , test "Extra html node results in Nothing" <|
            \_ ->
                Expect.equal Nothing (findTextChanges pWithImgHtmlNode pDomNode)
        , test "Extra dom node results in Nothing" <|
            \_ ->
                Expect.equal Nothing (findTextChanges pHtmlNode pWithImgDomNode)
        , test "Text changes" <|
            \_ ->
                Expect.equal (Just [ ( [ 0 ], "sample" ) ]) (findTextChanges pHtmlNodeDifferentText pDomNode)
        ]
