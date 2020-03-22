module RichTextEditor.Model.MarkDefinition exposing
    ( HtmlToMark
    , MarkDefinition
    , MarkToHtml
    , fromHtmlNode
    , markDefinition
    , name
    , toHtmlNode
    )

import RichTextEditor.Model.Internal.Spec


type alias MarkDefinition =
    RichTextEditor.Model.Internal.Spec.MarkDefinition


type alias MarkToHtml =
    RichTextEditor.Model.Internal.Spec.MarkToHtml


type alias HtmlToMark =
    RichTextEditor.Model.Internal.Spec.HtmlToMark


markDefinition : String -> MarkToHtml -> HtmlToMark -> MarkDefinition
markDefinition n toHtml fromHtml =
    RichTextEditor.Model.Internal.Spec.MarkDefinition
        { name = n
        , toHtmlNode = toHtml
        , fromHtmlNode = fromHtml
        }


name : MarkDefinition -> String
name d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.name


toHtmlNode : MarkDefinition -> MarkToHtml
toHtmlNode d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.toHtmlNode


fromHtmlNode : MarkDefinition -> HtmlToMark
fromHtmlNode d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.fromHtmlNode
