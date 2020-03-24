module RichTextEditor.Model.Decorations exposing
    ( Decorations, ElementDecoratorFunction, MarkDecoratorFunction, emptyDecorations, elementDecorators, markDecorators, withMarkDecorators, withElementDecorators
    , addElementDecoration, addMarkDecoration, selectableDecoration
    )

{-| Decorations are functions which add a list of Html.Attribute to rendered nodes and marks. They're
useful for adding things like event listeners and conditional styles/attributes
outside of the node and mark definitions.


# Decorations

@docs Decorations, ElementDecoratorFunction, MarkDecoratorFunction, emptyDecorations, elementDecorators, markDecorators, withMarkDecorators, withElementDecorators


# Helpers

@docs addElementDecoration, addMarkDecoration, selectableDecoration

-}

import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import RichTextEditor.Internal.Constants exposing (selection)
import RichTextEditor.Model.Editor exposing (InternalEditorMsg(..), Tagger)
import RichTextEditor.Model.Element exposing (Element, annotations)
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.MarkDefinition as MarkDefinition exposing (MarkDefinition)
import RichTextEditor.Model.Node exposing (Path)
import RichTextEditor.Model.NodeDefinition as NodeDefinition exposing (NodeDefinition)
import RichTextEditor.Model.Selection exposing (caretSelection)
import Set


{-| `Decorations` is a collection of mark and element decorators used in th editor view.


    decorations =
        emptyDecorations
            |> addElementDecoration image (selectableDecoration InternalMsg)

    --> a collection of decorations that contains a single element decoration for image

-}
type Decorations msg
    = Decorations (DecorationsContents msg)


type alias DecorationsContents msg =
    { marks : Dict String (List (MarkDecoratorFunction msg))
    , elements : Dict String (List (ElementDecoratorFunction msg))
    }


{-| `ElementDecoratorFunction` is a type alias for an element decoration function. The arguments
are as follows:

  - `editor path` is the path to this node from the editor root
  - `element` is the element to decorate
  - `relative html node path` is the relative path of the rendered html node. This is useful if you
    want to apply attributes to only a subset of the rendered html nodes of an element.

```
-- A decoration which adds an onclick listener to a checkbox in a custom checkbox item element
toggleCheckboxDecoration : Path -> Element -> Path -> List (Html.Attribute Msg)
toggleCheckboxDecoration editorNodePath element relativeHtmlNodePath =
    let
        checked =
            Maybe.withDefault False (findBoolAttribute "checked" (Element.attributes element))
    in
    if relativeHtmlNodePath == [ 0, 0 ] then
        [ Html.Events.onClick (ToggleCheckedTodoItem editorNodePath (not checked)) ]

    else
        []
```

-}
type alias ElementDecoratorFunction msg =
    Path -> Element -> Path -> List (Html.Attribute msg)


{-| `ElementDecoratorFunction` is a type alias for an mark decoration function. The arguments
are as follows:

  - `editor path` is the path to this node from the editor root
  - `mark` is the mark to decorate
  - `relative html node path` is the relative path of the rendered html node. This is useful if you
    want to apply attributes to only a subset of the rendered html nodes of an element.

```
-- A decoration that adds a highlight class attribute to rendered link marks
highlight : Path -> Mark -> Path -> List (Html.Attribute Msg)
highlight _ _ _ =
    [ Html.Attributes.class "highlight" ]
```

-}
type alias MarkDecoratorFunction msg =
    Path -> Mark -> Path -> List (Html.Attribute msg)


{-| Empty decorations
-}
emptyDecorations : Decorations msg
emptyDecorations =
    Decorations { marks = Dict.empty, elements = Dict.empty }


{-| A dictionary of mark name to a list of mark decorations.
-}
markDecorators : Decorations msg -> Dict String (List (MarkDecoratorFunction msg))
markDecorators d =
    case d of
        Decorations c ->
            c.marks


{-| A dictionary of element names to a list of mark decorations.
-}
elementDecorators : Decorations msg -> Dict String (List (ElementDecoratorFunction msg))
elementDecorators d =
    case d of
        Decorations c ->
            c.elements


{-| Creates a decorations object with the given mark decorations set
-}
withMarkDecorators : Dict String (List (MarkDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withMarkDecorators marks d =
    case d of
        Decorations c ->
            Decorations { c | marks = marks }


{-| Creates a decorations object with the given element decorations set
-}
withElementDecorators : Dict String (List (ElementDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withElementDecorators elements d =
    case d of
        Decorations c ->
            Decorations { c | elements = elements }


{-| Adds an element decoration for a defined node.


    decorations =
        emptyDecorations
            |> addElementDecoration image (selectableDecoration InternalMsg)

    --> a collection of decorations that contains a single element decoration for image

-}
addElementDecoration : NodeDefinition -> ElementDecoratorFunction msg -> Decorations msg -> Decorations msg
addElementDecoration definition decorator decorations =
    let
        eleDecorators =
            elementDecorators decorations

        name =
            NodeDefinition.name definition

        previousDecorations =
            Maybe.withDefault [] (Dict.get name eleDecorators)
    in
    decorations
        |> withElementDecorators (Dict.insert name (decorator :: previousDecorations) eleDecorators)


{-| Adds a mark decoration for a defined mark.


    decorations =
        emptyDecorations
            |> addMarkDecoration link highlight

    --> a collection of decorations that contains a single mark decoration for a link

-}
addMarkDecoration : MarkDefinition -> MarkDecoratorFunction msg -> Decorations msg -> Decorations msg
addMarkDecoration definition decorator decorations =
    let
        mDecorators =
            markDecorators decorations

        name =
            MarkDefinition.name definition

        previousDecorations =
            Maybe.withDefault [] (Dict.get name mDecorators)
    in
    decorations
        |> withMarkDecorators (Dict.insert name (decorator :: previousDecorations) mDecorators)


{-| Useful decoration for selectable elements. Adds an onclick listener on select, as well as a
rte-selected class if this item has the selected annotation.
-}
selectableDecoration : Tagger msg -> Path -> Element -> Path -> List (Html.Attribute msg)
selectableDecoration tagger editorNodePath elementParameters _ =
    (if Set.member selection (annotations elementParameters) then
        [ Html.Attributes.class "rte-selected" ]

     else
        []
    )
        ++ [ Html.Events.onClick
                (tagger <|
                    SelectionEvent (Just (caretSelection editorNodePath 0)) False
                )
           ]
