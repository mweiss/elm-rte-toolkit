module TestDomNode exposing (..)

import Array
import Expect
import RichText.Internal.DomNode exposing (DomNode(..), domElementNodeType, domTextNodeType, findTextChanges)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
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
                Expect.equal (Ok []) (findTextChanges pHtmlNode pDomNode)
        , test "Different type of node results in Error" <|
            \_ ->
                Expect.equal (Err "Dom node's tag was P, but I was expecting div") (findTextChanges divHtmlNode pDomNode)
        , test "Extra html node results in Error" <|
            \_ ->
                Expect.equal (Err "Dom node's children length was 1, but I was expecting 2") (findTextChanges pWithImgHtmlNode pDomNode)
        , test "Extra dom node results in Error" <|
            \_ ->
                Expect.equal (Err "Dom node's children length was 2, but I was expecting 1") (findTextChanges pHtmlNode pWithImgDomNode)
        , test "Finds text changes" <|
            \_ ->
                Expect.equal (Ok [ ( [ 0 ], "sample" ) ]) (findTextChanges pHtmlNodeDifferentText pDomNode)
        ]
