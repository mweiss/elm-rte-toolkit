module RichTextEditor.Model.Mark exposing (Mark, MarkOrder(..), ToggleAction(..), attributes, definition, mark, name, sort, toggle)

{-| A mark is a piece of information that can be attached to a node. It can be used to as extra
information when rendering a node (like color, font, and link information).
-}

import Dict exposing (Dict)
import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Spec as Spec exposing (MarkDefinition, attributesFromMark, nameFromMarkDefinition)


type alias Mark =
    Spec.Mark


mark : MarkDefinition -> List Attribute -> Mark
mark =
    Spec.mark


attributes : Mark -> List Attribute
attributes =
    attributesFromMark


definition : Mark -> MarkDefinition
definition =
    Spec.definitionFromMark


name : Mark -> String
name m =
    nameFromMarkDefinition (definition m)


type MarkOrder
    = MarkOrder Order


markOrder : Dict String Int -> MarkOrder
markOrder d =
    MarkOrder d


type alias Order =
    Dict String Int


sort : MarkOrder -> List Mark -> List Mark
sort order marks =
    case order of
        MarkOrder o ->
            List.sortBy
                (\m ->
                    ( Maybe.withDefault 0 <| Dict.get (name m) o
                    , name m
                    )
                )
                marks


type ToggleAction
    = Add
    | Remove
    | Flip


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
