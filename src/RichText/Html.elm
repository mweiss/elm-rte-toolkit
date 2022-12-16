module RichText.Html exposing (toHtml, toHtmlNode, fromHtml, fromHtmlNode, blockFromHtml)

{-| This module contains convenience functions for encoding and decoding editor nodes to and from
html. Its intent is to help developers who want to import and export editor state
as html.

@docs toHtml, toHtmlNode, fromHtml, fromHtmlNode, blockFromHtml

-}

import Array exposing (Array)
import Html.Parser exposing (Node(..), nodeToString)
import RichText.Config.Spec exposing (Spec)
import RichText.Internal.HtmlNode exposing (editorBlockNodeToHtmlNode)
import RichText.Internal.Spec exposing (htmlNodeToEditorFragment, htmlToElementArray)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Node exposing (Block)
import RichText.Node exposing (Fragment(..))


{-| Converts a block to an HtmlNode.

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

    (toHtmlNode markdown exampleBlock) == exampleHtmlNode
    --> True

-}
toHtmlNode : Spec -> Block -> HtmlNode
toHtmlNode =
    editorBlockNodeToHtmlNode


htmlNodeToNode : HtmlNode -> Node
htmlNodeToNode htmlNode =
    case htmlNode of
        ElementNode tag attrs children ->
            Element tag attrs (List.map htmlNodeToNode (Array.toList children))

        TextNode t ->
            Text t


{-| Converts a block to an html string.

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

    exampleHtml : String
    exampleHtml =
        "<div data-rte-doc=\"true\"><p>text<img src=\"logo.svg\">text2</p></div>"

    (toHtml markdown exampleBlock) == exampleHtml
    --> True

-}
toHtml : Spec -> Block -> String
toHtml spec block =
    nodeToString <| htmlNodeToNode <| toHtmlNode spec block


{-| Decodes an html string to an array of editor fragments, or returns an error if there was an
issue decoding the html.

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

    (Ok (Array.fromList [ exampleFragment ])) == (fromHtml markdown exampleHtml)
    --> True

-}
fromHtml : Spec -> String -> Result String (Array Fragment)
fromHtml =
    htmlToElementArray


{-| -}
fromHtmlNode : Spec -> HtmlNode -> Result String Fragment
fromHtmlNode spec =
    htmlNodeToEditorFragment spec []


{-| Convenience function that parses html and returns the first editor block that was decoded,
or an error if there was an issue decoding the html.

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

    exampleHtml : String
    exampleHtml =
        "<div data-rte-doc=\"true\"><p>text<img src=\"logo.svg\">text2</p></div>"

    (Ok exampleBlock) (blockFromHtml markdown exampleHtml)
    --> True

-}
blockFromHtml : Spec -> String -> Result String Block
blockFromHtml spec html =
    Result.andThen
        (\fragment ->
            case Array.get 0 fragment of
                Nothing ->
                    Err "There are no fragments to parse"

                Just f ->
                    case f of
                        BlockFragment bf ->
                            case Array.get 0 bf of
                                Nothing ->
                                    Err "Invalid initial fragment"

                                Just block ->
                                    Ok block

                        _ ->
                            Err "I was expecting a block, but instead I received an inline"
        )
        (htmlToElementArray spec html)
