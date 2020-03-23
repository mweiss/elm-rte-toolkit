module RichTextEditor.Model.Constants exposing (zeroWidthSpace)

{-| Miscellaneous constants used throughout the code

@docs zeroWidthSpace

-}


{-| A string representing the unicode character for a zero width space. Browsers remove empty
text nodes, so in order to keep the expected DOM structure and the real DOM structure consistent,
we use zero width spaces for empty html text nodes.
-}
zeroWidthSpace : String
zeroWidthSpace =
    "\u{200B}"
