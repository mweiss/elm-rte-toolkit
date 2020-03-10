module RichTextEditor.Model.HtmlNode exposing (HtmlAttribute, HtmlNode(..))

{-| HtmlNode is used to determine how to render the editor. We don't use the regular VirtualDOM library
because we can't inspect a node after it has been created. Note that we also don't allow text nodes
in the definition because this type is just for the structural content of the editor.
-}

import Array exposing (Array)


type HtmlNode
    = ElementNode String (List HtmlAttribute) (Array HtmlNode)
    | TextNode String


type alias HtmlAttribute =
    ( String, String )
