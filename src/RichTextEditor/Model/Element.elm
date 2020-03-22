module RichTextEditor.Model.Element exposing
    ( Element
    , annotations
    , attributes
    , comparableElement
    , definition
    , element
    , name
    , withAnnotations
    , withAttributes
    , withDefinition
    )

import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Internal.Spec as Spec
import RichTextEditor.Model.Spec exposing (NodeDefinition, nameFromNodeDefinition)
import Set exposing (Set)


type alias Element =
    Spec.Element


annotations : Element -> Set String
annotations =
    Spec.annotationsFromElement


attributes : Element -> List Attribute
attributes =
    Spec.attributesFromElement


definition : Element -> NodeDefinition
definition =
    Spec.definitionFromElement


name : Element -> String
name ele =
    nameFromNodeDefinition (definition ele)


element : NodeDefinition -> List Attribute -> Set String -> Element
element =
    Spec.element


withAnnotations : Set String -> Element -> Element
withAnnotations =
    Spec.elementWithAnnotations


withAttributes : List Attribute -> Element -> Element
withAttributes =
    Spec.elementWithAttributes


withDefinition : NodeDefinition -> Element -> Element
withDefinition =
    Spec.elementWithDefinition


comparableElement : Element -> ( String, List Attribute, Set String )
comparableElement p =
    ( name p
    , attributes p
    , annotations p
    )
