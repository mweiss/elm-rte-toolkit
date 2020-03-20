module RichTextEditor.Model.Spec exposing
    ( ElementToHtml
    , HtmlToElement
    , HtmlToMark
    , MarkDefinition
    , MarkToHtml
    , NodeDefinition
    , Spec
    , blockLeafContentType
    , blockNodeContentType
    , contentTypeFromNodeDefinition
    , emptySpec
    , fromHtmlNodeFromMarkDefinition
    , fromHtmlNodeFromNodeDefinition
    , groupFromNodeDefinition
    , inlineLeafContentType
    , markDefinition
    , markDefinitions
    , nameFromMarkDefinition
    , nameFromNodeDefinition
    , nodeDefinition
    , nodeDefinitions
    , textBlockContentType
    , toHtmlNodeFromMarkDefinition
    , toHtmlNodeFromNodeDefinition
    , withMarkDefinitions
    , withNodeDefinitions
    )

import RichTextEditor.Model.Internal.Spec
    exposing
        ( ContentType(..)
        , ElementToHtml
        , HtmlToElement
        , HtmlToMark
        , MarkDefinition(..)
        , MarkToHtml
        , NodeDefinition(..)
        )
import Set exposing (Set)


type alias NodeDefinition =
    RichTextEditor.Model.Internal.Spec.NodeDefinition


type alias MarkDefinition =
    RichTextEditor.Model.Internal.Spec.MarkDefinition


type alias MarkToHtml =
    RichTextEditor.Model.Internal.Spec.MarkToHtml


type alias HtmlToMark =
    RichTextEditor.Model.Internal.Spec.HtmlToMark


type alias ElementToHtml =
    RichTextEditor.Model.Internal.Spec.ElementToHtml


type alias HtmlToElement =
    RichTextEditor.Model.Internal.Spec.HtmlToElement


nodeDefinition : String -> String -> ContentType -> ElementToHtml -> HtmlToElement -> NodeDefinition
nodeDefinition name group contentType toHtml fromHtml =
    RichTextEditor.Model.Internal.Spec.NodeDefinition
        { name = name
        , group = group
        , toHtmlNode = toHtml
        , contentType = contentType
        , fromHtmlNode = fromHtml
        }


nameFromNodeDefinition : NodeDefinition -> String
nameFromNodeDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.name


groupFromNodeDefinition : NodeDefinition -> String
groupFromNodeDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.group


toHtmlNodeFromNodeDefinition : NodeDefinition -> ElementToHtml
toHtmlNodeFromNodeDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.toHtmlNode


fromHtmlNodeFromNodeDefinition : NodeDefinition -> HtmlToElement
fromHtmlNodeFromNodeDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.fromHtmlNode


contentTypeFromNodeDefinition : NodeDefinition -> ContentType
contentTypeFromNodeDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.contentType


markDefinition : String -> MarkToHtml -> HtmlToMark -> MarkDefinition
markDefinition name toHtml fromHtml =
    RichTextEditor.Model.Internal.Spec.MarkDefinition
        { name = name
        , toHtmlNode = toHtml
        , fromHtmlNode = fromHtml
        }


nameFromMarkDefinition : MarkDefinition -> String
nameFromMarkDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.name


toHtmlNodeFromMarkDefinition : MarkDefinition -> MarkToHtml
toHtmlNodeFromMarkDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.toHtmlNode


fromHtmlNodeFromMarkDefinition : MarkDefinition -> HtmlToMark
fromHtmlNodeFromMarkDefinition d =
    case d of
        RichTextEditor.Model.Internal.Spec.MarkDefinition c ->
            c.fromHtmlNode


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


markDefinitions : Spec -> List MarkDefinition
markDefinitions spec =
    case spec of
        Spec c ->
            c.marks


nodeDefinitions : Spec -> List NodeDefinition
nodeDefinitions spec =
    case spec of
        Spec c ->
            c.nodes


withMarkDefinitions : List MarkDefinition -> Spec -> Spec
withMarkDefinitions marks spec =
    case spec of
        Spec c ->
            Spec { c | marks = marks }


withNodeDefinitions : List NodeDefinition -> Spec -> Spec
withNodeDefinitions nodes spec =
    case spec of
        Spec c ->
            Spec { c | nodes = nodes }


type alias SpecContents =
    { marks : List MarkDefinition
    , nodes : List NodeDefinition
    }
