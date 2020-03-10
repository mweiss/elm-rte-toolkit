module RichTextEditor.Model.Spec exposing
    ( ContentType
    , ElementToHtml
    , HtmlToElement
    , HtmlToMark
    , MarkDefinition
    , MarkToHtml
    , NodeDefinition
    , Spec
    , emptySpec
    , withMarks
    , withNodes
    )

import Array exposing (Array)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode)
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node exposing (ElementParameters)
import Set exposing (Set)


type MarkDefinition
    = MarkDefinition MarkDefinitionContents


type alias MarkDefinitionContents =
    { name : String
    , toHtmlNode : MarkToHtml
    , fromHtmlNode : HtmlToMark
    }


type MarkToHtml
    = MarkToHtml MarkToHtmlContents


type alias MarkToHtmlContents =
    Mark -> Array HtmlNode -> HtmlNode


type HtmlToMark
    = HtmlToMark HtmlToMarkContents


type alias HtmlToMarkContents =
    HtmlNode -> Maybe ( Mark, Array HtmlNode )


type ElementToHtml
    = ElementToHtml ElementToHtmlContents


type alias ElementToHtmlContents =
    ElementParameters -> Array HtmlNode -> HtmlNode


type HtmlToElement
    = HtmlToElement HtmlToElementContents


type alias HtmlToElementContents =
    HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )


type NodeDefinition
    = NodeDefinition NodeDefinitionContents


type alias NodeDefinitionContents =
    { name : String
    , toHtmlNode : ElementToHtml
    , group : String
    , contentType : ContentType
    , fromHtmlNode : HtmlToElement
    }


nodeDefinition : String -> String -> ContentType -> ElementToHtml -> HtmlToElement -> NodeDefinition
nodeDefinition name group contentType toHtml fromHtml =
    NodeDefinition
        { name = name
        , group = group
        , toHtmlNode = toHtml
        , contentType = contentType
        , fromHtmlNode = fromHtml
        }


markDefinition : String -> MarkToHtml -> HtmlToMark -> MarkDefinition
markDefinition name toHtml fromHtml =
    MarkDefinition
        { name = name
        , toHtmlNode = toHtml
        , fromHtmlNode = fromHtml
        }


type ContentType
    = BlockNodeType (Maybe (Set String))
    | TextBlockNodeType (Maybe (Set String))
    | BlockLeafNodeType
    | InlineLeafNodeType


inlineLeafContentType : ContentType
inlineLeafContentType =
    InlineLeafNodeType


blockLeafContentType : ContentType
blockLeafContentType =
    BlockLeafNodeType


blockNodeContentType : List String -> ContentType
blockNodeContentType allowedGroups =
    BlockNodeType <|
        if List.isEmpty allowedGroups then
            Nothing

        else
            Just <| Set.fromList allowedGroups


textBlockContentType : List String -> ContentType
textBlockContentType allowedGroups =
    TextBlockNodeType <|
        if List.isEmpty allowedGroups then
            Nothing

        else
            Just <| Set.fromList allowedGroups


type Spec
    = Spec SpecContents


emptySpec =
    Spec { marks = [], nodes = [] }


withMarks : List MarkDefinition -> Spec -> Spec
withMarks marks spec =
    case spec of
        Spec c ->
            Spec { c | marks = marks }


withNodes : List NodeDefinition -> Spec -> Spec
withNodes nodes spec =
    case spec of
        Spec c ->
            Spec { c | nodes = nodes }


type alias SpecContents =
    { marks : List MarkDefinition
    , nodes : List NodeDefinition
    }
