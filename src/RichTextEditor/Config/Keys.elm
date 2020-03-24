module RichTextEditor.Config.Keys exposing (short, alt, meta, ctrl, shift, return, enter, backspace, delete)

{-| This module contains String constants related to defining keyboard commands.

@docs short, alt, meta, ctrl, shift, return, enter, backspace, delete

-}


{-| Platform specific modifier that is initialized on InternalEditorMsg.Init. It is equivalent
to meta on the mac/iOS and control on other platforms.
-}
short : String
short =
    "__Short__"


{-| Alt key
-}
alt : String
alt =
    "Alt"


{-| Meta key
-}
meta : String
meta =
    "Meta"


{-| Control key
-}
ctrl : String
ctrl =
    "Control"


{-| Shift key
-}
shift : String
shift =
    "Shift"


{-| Return key
-}
return : String
return =
    "Return"


{-| Enter key
-}
enter : String
enter =
    "Enter"


{-| Backspace key
-}
backspace : String
backspace =
    "Backspace"


{-| Delete key
-}
delete : String
delete =
    "Delete"
