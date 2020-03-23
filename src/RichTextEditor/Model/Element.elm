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

{-| An element represents the parameters of any non-text node.
-}

import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Internal.Spec as Spec
import RichTextEditor.Model.NodeDefinition as NodeDefinition exposing (NodeDefinition)
import Set exposing (Set)


{-| An `Element` represents the parameters of non-text nodes. It consists of a node definition,
a list of attributes, and a set of annotations.
-}
type alias Element =
    Spec.Element


{-| Creates an element. The arguments are as follows:

  - `node definition` is the `NodeDefinition` that defines this element.

  - `attributes` are a list of attributes, for example [StringAttribute 'src' 'logo.svg']

  - `annotations` is a set of annotations. Annotations are used to set flags on nodes for transforms
    or labeling purposes.

```
    element header [IntegerAttribute "level" 1] Set.empty
    --> creates a header element
```

-}
element : NodeDefinition -> List Attribute -> Set String -> Element
element =
    Spec.element


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
    NodeDefinition.name (definition ele)


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
