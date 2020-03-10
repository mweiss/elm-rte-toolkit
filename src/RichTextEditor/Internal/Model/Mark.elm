module RichTextEditor.Internal.Model.Mark exposing (Mark, mark)

{-| A mark is a piece of information that can be attached to a node. It can be used to as extra
information when rendering a node (like color, font, and link information).
-}

import Dict exposing (Dict)
import RichTextEditor.Internal.Model.Attribute exposing (Attribute)


type Mark
    = Mark Contents


type alias Contents =
    { name : String, attributes : List Attribute }


mark : String -> List Attribute -> Mark
mark name attributes =
    Mark { name = name, attributes = attributes }


type MarkOrder
    = MarkOrder Order


markOrder : Dict String Int -> MarkOrder
markOrder d =
    MarkOrder d


type alias Order =
    Dict String Int
