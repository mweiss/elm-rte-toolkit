module Rte.DOMNode exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE


{-| The DOM text node nodeType value as specified by the w3c spec [w3c spec][w3c-custom-types-text-node]
[w3c-custom-types-text-node]: <https://www.w3.org/TR/domcore/#dom-node-text_node>
-}
domTextNodeType =
    3


{-| The DOM element node nodeType value as specified by the [w3c spec][w3c-custom-types-element-node]
[w3c-custom-types-element-node]: <https://www.w3.org/TR/domcore/#dom-node-element_node>
-}
domElementNodeType =
    1


{-| A minimal representation of DOMNode. It's purpose is to validate the contents of the DOM for any
unexpected structural changes that can happen in a contenteditable node before applying changes that may
effect to the virtual DOM.
-}
type alias DOMNodeContents =
    { nodeType : Int
    , tagName : Maybe String
    , nodeValue : Maybe String
    , childNodes : Maybe (List DOMNode)
    }


{-| A minimal representation of a DOMNode. Since the structure of DOMNodeContents is recursive,
we need to define a literal type to avoid infinite recursion.
-}
type DOMNode
    = DOMNode DOMNodeContents


decodeDOMNode : D.Decoder DOMNode
decodeDOMNode =
    D.map DOMNode
        (D.map4 DOMNodeContents
            (D.field "nodeType" D.int)
            (D.maybe (D.field "tagName" D.string))
            (D.maybe (D.field "nodeValue" D.string))
            (D.maybe (D.field "childNodes" (DE.collection (D.lazy (\_ -> decodeDOMNode)))))
        )



{-
   This method extracts the editor nodes by taking the first child of the root node,
   and then the first child of that node.  The DOMNodes we receive from the change event start
   at the beginning of the content editable, and it should contain one immediate child node that
   contains the rendered editor nodes.
-}


extractRootEditorBlockNode : DOMNode -> Maybe DOMNode
extractRootEditorBlockNode domNode =
    case domNode of
        DOMNode node ->
            case node.childNodes of
                Nothing ->
                    Nothing

                Just childNodes ->
                    List.head childNodes
