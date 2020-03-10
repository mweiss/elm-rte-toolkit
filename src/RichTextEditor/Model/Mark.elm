module RichTextEditor.Model.Mark exposing (Mark, MarkOrder, attributes, mark, name)

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
