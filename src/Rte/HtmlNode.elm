module Rte.HtmlNode exposing (..)

import Rte.Model exposing (ChildNodes(..), EditorBlockNode, HtmlNode(..), Spec)


editorBlockNodeToHtmlNode : Spec -> EditorBlockNode -> HtmlNode
editorBlockNodeToHtmlNode spec node =
    TextNode ""


editorInlineLeafToHtmlNode : Spec -> EditorBlockNode -> HtmlNode
editorInlineLeafToHtmlNode spec node =
    TextNode ""
