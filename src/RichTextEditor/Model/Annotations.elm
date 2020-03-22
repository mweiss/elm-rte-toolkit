module RichTextEditor.Model.Annotations exposing (selection, selectable, lift)

{-| This module contains common constants used as node annotations.

@docs selection, selectable, lift

-}


{-| Represents that a node is currently selected. This annotation is transient, e.g. it
should be cleared before a transform or command is complete. This annotation is also used when
rendering to annotate a selected node for decorators.
-}
selection : String
selection =
    "__selection__"


{-| Represents that a node is can be selected. This annotation is not transient.
-}
selectable : String
selectable =
    "__selectable__"


{-| Represents that a node is can be selected. This annotation is transient, e.g. it should be
cleared before a transform or command is complete.
-}
lift : String
lift =
    "__lift__"
