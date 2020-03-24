module RichTextEditor.Internal.Constants exposing (zeroWidthSpace, selection, selectable, lift)

{-| Miscellaneous constants used throughout the code

@docs zeroWidthSpace, selection, selectable, lift

-}


{-| A string representing the unicode character for a zero width space. Browsers remove empty
text nodes, so in order to keep the expected DOM structure and the real DOM structure consistent,
we use zero width spaces for empty html text nodes.
-}
zeroWidthSpace : String
zeroWidthSpace =
    "\u{200B}"


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
