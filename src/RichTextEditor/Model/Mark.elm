module RichTextEditor.Model.Mark exposing
    ( Mark, mark, name, attributes, withAttributes
    , MarkOrder, markOrderFromSpec
    , sort, ToggleAction(..), toggle
    )

{-| A mark is a piece of information that can be attached to an inline node (like color, font, and link information).


# Mark

@docs Mark, mark, name, attributes, withAttributes


# Mark order

@docs MarkOrder, markOrderFromSpec


# List helpers

@docs sort, ToggleAction, toggle

-}

import Dict exposing (Dict)
import RichTextEditor.Internal.Model.Definitions as Internal exposing (MarkDefinition, attributesFromMark)
import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.MarkDefinition as MarkDefinition
import RichTextEditor.Model.Spec exposing (Spec, markDefinitions)


{-| A mark is a piece of information defined by a `MarkDefinition` that can be attached to an inline node,
like color font, or link information.
-}
type alias Mark =
    Internal.Mark


{-| Creates a mark. The arguments are as follows:

  - `definition` is the `MarkDefinition` that defines this node. Note that even though marks require a mark definition,
    it's still safe to use (==) because the function arguments are not stored on the Mark

  - `attributes` are a list of attributes, for example [StringAttribute 'href' 'www.google.com']

```
    mark link [StringAttribute 'href' 'www.google.com']
    --> creates a link mark
```

-}
mark : MarkDefinition -> List Attribute -> Mark
mark =
    Internal.mark


{-| A list of attributes associated with the mark
-}
attributes : Mark -> List Attribute
attributes =
    attributesFromMark


{-| Name of the mark
-}
name : Mark -> String
name =
    Internal.nameFromMark


{-| Creates a mark with new attributes
-}
withAttributes : List Attribute -> Mark -> Mark
withAttributes =
    Internal.markWithAttributes


{-| A mark order is a way of sorting marks. In order to have a single way of representing a document,
it's necessary to make sure marks are sorted in a consistent way. Mark orders should be derived
from the spec.

    markOrderFromSpec spec
    --> returns a mark order that can be used to sort a list of marks

-}
type MarkOrder
    = MarkOrder Order


markOrder : Dict String Int -> MarkOrder
markOrder d =
    MarkOrder d


type alias Order =
    Dict String Int


{-| Sorts a list of marks from the given `MarkOrder`

    spec : Spec
    spec =
        emptySpec
            |> withMarkDefinitions
                [ bold
                , italic
                ]

    sort (markOrderFromSpec spec) [mark italic [], mark bold []]
    --> [mark bold [], mark bold []]

-}
sort : MarkOrder -> List Mark -> List Mark
sort order marks =
    case order of
        MarkOrder o ->
            List.sortBy
                (\m ->
                    ( Maybe.withDefault -1 <| Dict.get (name m) o
                    , name m
                    )
                )
                marks


{-| Actions used to toggle a mark from a list of marks.

  - `Add` adds or replaces a mark if it exists.

  - `Remove` removes all marks with the same name if it exists.

  - `Flip` will remove a mark if a mark with the same name exists, otherwise it will add it.

-}
type ToggleAction
    = Add
    | Remove
    | Flip


{-| Returns a list that executes the toggle action and maintains the given mark order.

    -- Assuming the mark order is bold < italic < code:
    toggle Add (markOrderFromSpec spec) (mark code []) [mark bold [], mark italic []]
    --> [mark bold [], mark italic [], mark code []]

-}
toggle : ToggleAction -> MarkOrder -> Mark -> List Mark -> List Mark
toggle toggleAction order mark_ marks =
    let
        isMember =
            List.any (\m -> name m == name mark_) marks
    in
    if toggleAction == Remove || (toggleAction == Flip && isMember) then
        List.filter (\x -> name x /= name mark_) marks

    else if not isMember then
        sort order (mark_ :: marks)

    else
        List.map
            (\x ->
                if name x == name mark_ then
                    mark_

                else
                    x
            )
            marks


{-| Derives the mark order from the order of the spec's mark definitions, e.g. `(markDefinitions spec)`.

    spec : Spec
    spec =
        emptySpec
            |> withMarkDefinitions
                [ link
                , bold
                , italic
                , code
                ]

    markOrderFromSpec spec
    --> returns a mark order that will sort marks in the following order:  link, bold, italic, and code.

-}
markOrderFromSpec : Spec -> MarkOrder
markOrderFromSpec spec =
    MarkOrder <| Dict.fromList (List.indexedMap (\i m -> ( MarkDefinition.name m, i )) (markDefinitions spec))
