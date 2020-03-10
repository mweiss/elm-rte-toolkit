module RichTextEditor.Internal.Model.DomNode exposing (DomNode(..), DomNodeContents)

import Array exposing (Array)


{-| A minimal representation of DomNode. It's purpose is to validate the contents of the DOM for any
unexpected structural changes that can happen in a contenteditable node before applying changes that may
effect to the virtual DOM.
-}
type alias DomNodeContents =
    { nodeType : Int
    , tagName : Maybe String
    , nodeValue : Maybe String
    , childNodes : Maybe (Array DomNode)
    }


{-| A minimal representation of a DomNode. Since the structure of DomNodeContents is recursive,
we need to define a literal type to avoid infinite recursion.
-}
type DomNode
    = DomNode DomNodeContents
