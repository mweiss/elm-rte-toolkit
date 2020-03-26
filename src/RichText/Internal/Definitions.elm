module RichText.Internal.Definitions exposing (..)

{-| Internal module for various entities related to definition, elements and nodes to keep records
private and avoid dependency loops.
-}

import Array exposing (Array)
import RichText.Model.Attribute exposing (Attribute)
import RichText.Model.HtmlNode exposing (HtmlNode)
import Set exposing (Set)


{-| Implementation note:

We only store the name in element parameters and marks because we want to follow the Elm
architecture and not store functions in the model.

The benefits of this is that it avoids serialization issues with the debugger and potential
runtime errors on (==). The tradeoff to not storing the serialization functions directly in the
model is an extra dictionary lookup for each node everytime we want to get the definition, as
well as a somewhat more annoying API where functions need an extra spec argument so we can lookup
the definition.

-}
type ContentType
    = BlockNodeType (Maybe (Set String))
    | TextBlockNodeType (Maybe (Set String))
    | BlockLeafNodeType
    | InlineLeafNodeType


type alias ElementParametersContents =
    { name : String
    , attributes : List Attribute
    , annotations : Set String
    }


type Element
    = ElementParameters ElementParametersContents


element : NodeDefinition -> List Attribute -> Element
element def attrs =
    case def of
        NodeDefinition d ->
            ElementParameters { name = d.name, attributes = attrs, annotations = Set.empty }


nameFromElement : Element -> String
nameFromElement parameters =
    case parameters of
        ElementParameters c ->
            c.name


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


elementWithAnnotations : Set String -> Element -> Element
elementWithAnnotations annotations parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | annotations = annotations }


elementWithAttributes : List Attribute -> Element -> Element
elementWithAttributes attrs parameters =
    case parameters of
        ElementParameters c ->
            ElementParameters <| { c | attributes = attrs }


type Mark
    = Mark Contents


type alias Contents =
    { name : String, attributes : List Attribute }


mark : MarkDefinition -> List Attribute -> Mark
mark n a =
    case n of
        MarkDefinition nn ->
            Mark { name = nn.name, attributes = a }


nameFromMark : Mark -> String
nameFromMark m =
    case m of
        Mark c ->
            c.name


attributesFromMark : Mark -> List Attribute
attributesFromMark m =
    case m of
        Mark c ->
            c.attributes


markWithAttributes : List Attribute -> Mark -> Mark
markWithAttributes attributes_ m =
    case m of
        Mark c ->
            Mark { c | attributes = attributes_ }


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


toStringContentType : ContentType -> String
toStringContentType contentType =
    case contentType of
        TextBlockNodeType _ ->
            "TextBlockNodeType"

        InlineLeafNodeType ->
            "InlineLeafNodeType"

        BlockNodeType _ ->
            "BlockNodeType"

        BlockLeafNodeType ->
            "BlockLeafNodeType"
