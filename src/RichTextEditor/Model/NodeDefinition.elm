module RichTextEditor.Model.NodeDefinition exposing
    ( ContentType
    , ElementToHtml
    , HtmlToElement
    , NodeDefinition
    , blockLeaf
    , blockNode
    , contentType
    , fromHtmlNode
    , group
    , inlineLeaf
    , name
    , nodeDefinition
    , textBlock
    , toHtmlNode
    )

import RichTextEditor.Model.Internal.Spec exposing (ContentType(..))
import Set


type alias ContentType =
    RichTextEditor.Model.Internal.Spec.ContentType


type alias NodeDefinition =
    RichTextEditor.Model.Internal.Spec.NodeDefinition


type alias ElementToHtml =
    RichTextEditor.Model.Internal.Spec.ElementToHtml


type alias HtmlToElement =
    RichTextEditor.Model.Internal.Spec.HtmlToElement


nodeDefinition : String -> String -> ContentType -> ElementToHtml -> HtmlToElement -> NodeDefinition
nodeDefinition n g c toHtml fromHtml =
    RichTextEditor.Model.Internal.Spec.NodeDefinition
        { name = n
        , group = g
        , toHtmlNode = toHtml
        , contentType = c
        , fromHtmlNode = fromHtml
        }


name : NodeDefinition -> String
name d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.name


group : NodeDefinition -> String
group d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.group


toHtmlNode : NodeDefinition -> ElementToHtml
toHtmlNode d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.toHtmlNode


fromHtmlNode : NodeDefinition -> HtmlToElement
fromHtmlNode d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.fromHtmlNode


contentType : NodeDefinition -> ContentType
contentType d =
    case d of
        RichTextEditor.Model.Internal.Spec.NodeDefinition c ->
            c.contentType


inlineLeaf : ContentType
inlineLeaf =
    InlineLeafNodeType


blockLeaf : ContentType
blockLeaf =
    BlockLeafNodeType


blockNode : List String -> ContentType
blockNode allowedGroups =
    BlockNodeType <|
        if List.isEmpty allowedGroups then
            Nothing

        else
            Just <| Set.fromList allowedGroups


textBlock : List String -> ContentType
textBlock allowedGroups =
    TextBlockNodeType <|
        if List.isEmpty allowedGroups then
            Nothing

        else
            Just <| Set.fromList allowedGroups
