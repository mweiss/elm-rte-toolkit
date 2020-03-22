module RichTextEditor.Model.Internal.Spec exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode)
import Set exposing (Set)


type ContentType
    = BlockNodeType (Maybe (Set String))
    | TextBlockNodeType (Maybe (Set String))
    | BlockLeafNodeType
    | InlineLeafNodeType


type alias ElementParametersContents =
    { definition : NodeDefinition
    , attributes : List Attribute
    , annotations : Set String
    }


type Element
    = ElementParameters ElementParametersContents


elementParameters : NodeDefinition -> List Attribute -> Set String -> Element
elementParameters def attrs annotations =
    ElementParameters { definition = def, attributes = attrs, annotations = annotations }


definitionFromElement : Element -> NodeDefinition
definitionFromElement parameters =
    case parameters of
        ElementParameters c ->
            c.definition


attributesFromElement : Element -> List Attribute
attributesFromElement parameters =
    case parameters of
        ElementParameters c ->
            c.attributes


annotationsFromElement : Element -> Set String
annotationsFromElement parameters =
    case parameters of
        ElementParameters c ->
            c.annotations


elementParametersWithAnnotations : Set String -> Element -> Element
elementParametersWithAnnotations annotations parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | annotations = annotations }


elementParametersWithDefinition : NodeDefinition -> Element -> Element
elementParametersWithDefinition d parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | definition = d }


elementParametersWithAttributes : List Attribute -> Element -> Element
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
    Element -> Array HtmlNode -> HtmlNode


type alias HtmlToElement =
    NodeDefinition -> HtmlNode -> Maybe ( Element, Array HtmlNode )


type NodeDefinition
    = NodeDefinition NodeDefinitionContents


type alias NodeDefinitionContents =
    { name : String
    , toHtmlNode : ElementToHtml
    , group : String
    , contentType : ContentType
    , fromHtmlNode : HtmlToElement
    }
