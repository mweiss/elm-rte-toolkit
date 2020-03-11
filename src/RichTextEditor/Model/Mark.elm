module RichTextEditor.Model.Mark exposing (Mark, MarkOrder(..), ToggleAction(..), attributes, mark, name, sort, toggle)

{-| A mark is a piece of information that can be attached to a node. It can be used to as extra
information when rendering a node (like color, font, and link information).
-}

import Dict exposing (Dict)
import RichTextEditor.Model.Attribute exposing (Attribute)


type Mark
    = Mark Contents


type alias Contents =
    { name : String, attributes : List Attribute }


mark : String -> List Attribute -> Mark
mark n a =
    Mark { name = n, attributes = a }


name : Mark -> String
name m =
    case m of
        Mark c ->
            c.name


attributes : Mark -> List Attribute
attributes m =
    case m of
        Mark c ->
            c.attributes


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
                (\m -> ( Maybe.withDefault 0 <| Dict.get (name m) o, name m ))
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
