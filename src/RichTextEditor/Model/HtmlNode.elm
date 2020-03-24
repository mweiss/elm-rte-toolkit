module RichTextEditor.Model.HtmlNode exposing (HtmlNode(..), HtmlAttribute)

{-| `HtmlNode` is used to determine how to render the editor. We don't use the built in Html library
because we can't inspect a node after it has been created.

@docs HtmlNode, HtmlAttribute

-}

import Array exposing (Array)


{-| An HTML node. It can be either an `ElementNode` or `TextNode`

    ElementNode "p" [ ( "class", "my-paragraph" ) ] (Array.fromList [ Text "sample" ])

-}
type HtmlNode
    = ElementNode String (List HtmlAttribute) (Array HtmlNode)
    | TextNode String


{-| An HTML attribute:

    ( "src", "logo.svg" )

-}
type alias HtmlAttribute =
    ( String, String )
