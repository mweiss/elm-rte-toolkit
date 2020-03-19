module RichTextEditor.Model.Spec exposing
    ( ContentType(..)
    , ElementParameters
    , ElementToHtml
    , HtmlToElement
    , HtmlToMark
    , Mark
    , MarkDefinition
    , MarkToHtml
    , NodeDefinition
    , Spec
    , annotationsFromElementParameters
    , attributesFromElementParameters
    , attributesFromMark
    , blockLeafContentType
    , blockNodeContentType
    , contentTypeFromNodeDefinition
    , definitionFromElementParameters
    , definitionFromMark
    , elementParameters
    , elementParametersWithAnnotations
    , elementParametersWithAttributes
    , elementParametersWithDefinition
    , emptySpec
    , fromHtmlNodeFromMarkDefinition
    , fromHtmlNodeFromNodeDefinition
    , groupFromNodeDefinition
    , inlineLeafContentType
    , mark
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

import Array exposing (Array)
import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode)
import Set exposing (Set)


type alias ElementParametersContents =
    { definition : NodeDefinition
    , attributes : List Attribute
    , annotations : Set String
    }


type ElementParameters
    = ElementParameters ElementParametersContents


elementParameters : NodeDefinition -> List Attribute -> Set String -> ElementParameters
elementParameters def attrs annotations =
    ElementParameters { definition = def, attributes = attrs, annotations = annotations }


definitionFromElementParameters : ElementParameters -> NodeDefinition
definitionFromElementParameters parameters =
    case parameters of
        ElementParameters c ->
            c.definition


attributesFromElementParameters : ElementParameters -> List Attribute
attributesFromElementParameters parameters =
    case parameters of
        ElementParameters c ->
            c.attributes


annotationsFromElementParameters : ElementParameters -> Set String
annotationsFromElementParameters parameters =
    case parameters of
        ElementParameters c ->
            c.annotations


elementParametersWithAnnotations : Set String -> ElementParameters -> ElementParameters
elementParametersWithAnnotations annotations parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | annotations = annotations }


elementParametersWithDefinition : NodeDefinition -> ElementParameters -> ElementParameters
elementParametersWithDefinition d parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | definition = d }


elementParametersWithAttributes : List Attribute -> ElementParameters -> ElementParameters
elementParametersWithAttributes attrs parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | attributes = attrs }


type Mark
    = Mark Contents


type alias Contents =
    { definition : MarkDefinition, attributes : List Attribute }


mark : MarkDefinition -> List Attribute -> Mark
mark n a =
    Mark { definition = n, attributes = a }


definitionFromMark : Mark -> MarkDefinition
definitionFromMark m =
    case m of
        Mark c ->
            c.definition


attributesFromMark : Mark -> List Attribute
attributesFromMark m =
    case m of
        Mark c ->
            c.attributes


type MarkDefinition
    = MarkDefinition MarkDefinitionContents


type alias MarkDefinitionContents =
    { name : String
    , toHtmlNode : MarkToHtml
    , fromHtmlNode : HtmlToMark
    }


type alias MarkToHtml =
    Mark -> Array HtmlNode -> HtmlNode


type alias HtmlToMark =
    MarkDefinition -> HtmlNode -> Maybe ( Mark, Array HtmlNode )


type alias ElementToHtml =
    ElementParameters -> Array HtmlNode -> HtmlNode


type alias HtmlToElement =
    NodeDefinition -> HtmlNode -> Maybe ( ElementParameters, Array HtmlNode )


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


nameFromNodeDefinition : NodeDefinition -> String
nameFromNodeDefinition d =
    case d of
        NodeDefinition c ->
            c.name


groupFromNodeDefinition : NodeDefinition -> String
groupFromNodeDefinition d =
    case d of
        NodeDefinition c ->
            c.group


toHtmlNodeFromNodeDefinition : NodeDefinition -> ElementToHtml
toHtmlNodeFromNodeDefinition d =
    case d of
        NodeDefinition c ->
            c.toHtmlNode


fromHtmlNodeFromNodeDefinition : NodeDefinition -> HtmlToElement
fromHtmlNodeFromNodeDefinition d =
    case d of
        NodeDefinition c ->
            c.fromHtmlNode


contentTypeFromNodeDefinition : NodeDefinition -> ContentType
contentTypeFromNodeDefinition d =
    case d of
        NodeDefinition c ->
            c.contentType


markDefinition : String -> MarkToHtml -> HtmlToMark -> MarkDefinition
markDefinition name toHtml fromHtml =
    MarkDefinition
        { name = name
        , toHtmlNode = toHtml
        , fromHtmlNode = fromHtml
        }


nameFromMarkDefinition : MarkDefinition -> String
nameFromMarkDefinition d =
    case d of
        MarkDefinition c ->
            c.name


toHtmlNodeFromMarkDefinition : MarkDefinition -> MarkToHtml
toHtmlNodeFromMarkDefinition d =
    case d of
        MarkDefinition c ->
            c.toHtmlNode


fromHtmlNodeFromMarkDefinition : MarkDefinition -> HtmlToMark
fromHtmlNodeFromMarkDefinition d =
    case d of
        MarkDefinition c ->
            c.fromHtmlNode


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
