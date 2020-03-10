module RichTextEditor.Model.Event exposing (..)

import Json.Encode as E
import RichTextEditor.Model.Node exposing (NodePath)
import RichTextEditor.Model.Selection exposing (Selection)


{-| Whenever the elm-editor MutationObserver detects a change, it triggers an editor change event
that the editor has to respond to. Note that it's very important for the editor to respond to every
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


type alias PasteEvent =
    { text : String
    , html : String
    }


{-|

    A represents a text change at the given path in a editor node or DOM tree.

-}
type alias TextChange =
    ( NodePath, String )
