module TestHtml exposing (..)

{-| TODO: add a lot more tests, this right now only covers the documentation example.
-}

import Array
import Expect
import RichText.Definitions exposing (doc, image, markdown, paragraph)
import RichText.Html exposing (blockFromHtml, fromHtml, toHtml, toHtmlNode)
import RichText.Model.Attribute exposing (Attribute(..))
import RichText.Model.Element as Element
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Node exposing (Block, Children(..), Inline, block, blockChildren, inlineChildren, inlineElement, plainText)
import RichText.Node exposing (Fragment(..))
import Test exposing (..)


exampleBlock : Block
exampleBlock =
    block
        (Element.element doc [])
        (blockChildren <|
            Array.fromList
                [ block
                    (Element.element paragraph [])
                    (inlineChildren <|
                        Array.fromList
                            [ plainText "text"
                            , inlineElement (Element.element image [ StringAttribute "src" "logo.svg" ]) []
                            , plainText "text2"
                            ]
                    )
                ]
        )


exampleFragment : Fragment
exampleFragment =
    BlockFragment <| Array.fromList [ exampleBlock ]


exampleHtml : String
exampleHtml =
    "<div data-rte-doc=\"true\"><p>text<img src=\"logo.svg\">text2</p></div>"


exampleHtmlNode : HtmlNode
exampleHtmlNode =
    ElementNode "div" [ ( "data-rte-doc", "true" ) ] <|
        Array.fromList
            [ ElementNode "p" [] <|
                Array.fromList
                    [ TextNode "text"
                    , ElementNode "img" [ ( "src", "logo.svg" ) ] Array.empty
                    , TextNode "text2"
                    ]
            ]


testToHtml : Test
testToHtml =
    describe "Tests the toHtml function"
        [ test "Make sure the example works as expected" <|
            \_ ->
                Expect.equal exampleHtml (toHtml markdown exampleBlock)
        ]


testToHtmlNode : Test
testToHtmlNode =
    describe "Tests the toHtmlNode function"
        [ test "Make sure the example works as expected" <|
            \_ ->
                Expect.equal exampleHtmlNode (toHtmlNode markdown exampleBlock)
        ]


testFromHtml : Test
testFromHtml =
    describe "Tests the fromHtml function"
        [ test "Make sure the example works as expected" <|
            \_ ->
                Expect.equal (Ok (Array.fromList [ exampleFragment ])) (fromHtml markdown exampleHtml)
        ]


testBlockFromHtml : Test
testBlockFromHtml =
    describe "Tests the blockFromHtml function"
        [ test "Make sure the example works as expected" <|
            \_ ->
                Expect.equal (Ok exampleBlock) (blockFromHtml markdown exampleHtml)
        ]
