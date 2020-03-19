module ExtraMarks exposing (..)

import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Spec
    exposing
        ( HtmlToMark
        , MarkDefinition
        , MarkToHtml
        , markDefinition
        )
import RichTextEditor.Spec exposing (defaultHtmlToMark)


underline : MarkDefinition
underline =
    markDefinition "underline" underlineToHtmlNode htmlNodeToUnderline


underlineToHtmlNode : MarkToHtml
underlineToHtmlNode _ children =
    ElementNode "u" [] children


htmlNodeToUnderline : HtmlToMark
htmlNodeToUnderline =
    defaultHtmlToMark "u"


strikethrough : MarkDefinition
strikethrough =
    markDefinition "strikethrough" strikethroughToHtmlNode htmlNodeToStrikethrough


strikethroughToHtmlNode : MarkToHtml
strikethroughToHtmlNode _ children =
    ElementNode "s" [] children


htmlNodeToStrikethrough : HtmlToMark
htmlNodeToStrikethrough =
    defaultHtmlToMark "s"
