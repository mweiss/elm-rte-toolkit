module Rte.List exposing (..)

import Rte.Model exposing (Command, ElementParameters, elementParameters)


type ListType
    = Ordered
    | Unordered


type alias ListDefinition =
    { ordered : ElementParameters, unordered : ElementParameters, item : ElementParameters }


defaultListDefinition : ListDefinition
defaultListDefinition =
    { ordered = elementParameters "ol" [] []
    , unordered = elementParameters "ul" [] []
    , item = elementParameters "li" [] []
    }


wrap : ListDefinition -> ListType -> Command
wrap definition type_ editorState =
    Err "Not implemented"


split : ListDefinition -> Command
split definition editorState =
    Err "Not implemented"


lift : ListDefinition -> Command
lift definition editorState =
    Err "Not implemented"


joinBackward : ListDefinition -> Command
joinBackward definition editorState =
    Err "Not implemented"


joinForward : ListDefinition -> Command
joinForward definition editorState =
    Err "Not implemented"
