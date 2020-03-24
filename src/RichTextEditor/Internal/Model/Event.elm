module RichTextEditor.Internal.Model.Event exposing (EditorChange, InitEvent, InputEvent, KeyboardEvent, PasteEvent, TextChange)

{-| This module holds the records used for decoded events like input, keyboard, as well
as a few custom events.
-}

import Json.Encode as E
import RichTextEditor.Model.Node exposing (Path)
import RichTextEditor.Model.Selection exposing (Selection)


{-| Whenever the elm-editor MutationObserver detects a change, it triggers an editor change event
that the editor has to respond to. Note that it's important for the editor to respond to every
change event so that the VirtualDOM doesn't try to render when the DOM is not in the state that
it's expecting.
-}
type alias EditorChange =
    { root : E.Value
    , selection : Maybe Selection
    , characterDataMutations : Maybe (List TextChange)
    }


{-| The attributes parsed from an input event.
-}
type alias InputEvent =
    { data : Maybe String, isComposing : Bool, inputType : String }


{-| The attributes parsed from a keyboard event.
-}
type alias KeyboardEvent =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    , isComposing : Bool
    }


{-| The attributes parsed from a `pastewithdata` event.
-}
type alias PasteEvent =
    { text : String
    , html : String
    }


{-| The attributes parsed from an `editorinit` event.
-}
type alias InitEvent =
    { shortKey : String
    }


{-| A represents a text change at the given path in a editor node or DOM tree. The string provided
is the new text at that path.
-}
type alias TextChange =
    ( Path, String )
