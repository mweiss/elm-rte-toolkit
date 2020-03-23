module RichTextEditor.Model.Element exposing
    ( Element
    , annotations
    , attributes
    , comparableElement
    , element
    , name
    , withAnnotations
    , withAttributes
    )

{-| An element represents the parameters of any non-text node.
-}

import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Internal as Internal
import RichTextEditor.Model.NodeDefinition exposing (NodeDefinition)
import Set exposing (Set)


{-| An `Element` represents the parameters of non-text nodes. It consists of a node definition,
a list of attributes, and a set of annotations.
-}
type alias Element =
    Internal.Element


{-| Creates an element. The arguments are as follows:

  - `node definition` is the `NodeDefinition` that defines this element. Note that even though
    elements require a node definition, it's still safe to use (==) because the function arguments
    are not stored.

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
    Internal.element


annotations : Element -> Set String
annotations =
    Internal.annotationsFromElement


attributes : Element -> List Attribute
attributes =
    Internal.attributesFromElement


name : Element -> String
name =
    Internal.nameFromElement


withAnnotations : Set String -> Element -> Element
withAnnotations =
    Internal.elementWithAnnotations


withAttributes : List Attribute -> Element -> Element
withAttributes =
    Internal.elementWithAttributes


comparableElement : Element -> ( String, List Attribute, Set String )
comparableElement p =
    ( name p
    , attributes p
    , annotations p
    )
