module RichText.Internal.DomNode exposing
    ( DomNode(..)
    , decodeDomNode
    , domElementNodeType
    , domTextNodeType
    , extractRootEditorBlockNode
    , findTextChanges
    )

import Array exposing (Array)
import Array.Extra
import Json.Decode as D
import Json.Decode.Extra as DE
import RichText.Internal.Constants exposing (zeroWidthSpace)
import RichText.Internal.Event exposing (TextChange)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.Node exposing (Path)


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


decodeDomNode : D.Decoder DomNode
decodeDomNode =
    D.map DomNode
        (D.map4 DomNodeContents
            (D.field "nodeType" D.int)
            (D.maybe (D.field "tagName" D.string))
            (D.maybe (D.field "nodeValue" D.string))
            (D.maybe (D.field "childNodes" (D.map Array.fromList (DE.collection (D.lazy (\_ -> decodeDomNode))))))
        )



{-
   This method extracts the editor nodes by taking the first child of the root node,
   and then the first child of that node.  The DomNodes we receive from the change event start
   at the beginning of the content editable, and it should contain one immediate child node that
   contains the rendered editor nodes.
-}


extractRootEditorBlockNode : DomNode -> Maybe DomNode
extractRootEditorBlockNode domNode =
    case domNode of
        DomNode node ->
            case node.childNodes of
                Nothing ->
                    Nothing

                Just childNodes ->
                    Array.get 0 childNodes


findTextChanges : HtmlNode -> DomNode -> Result String (List TextChange)
findTextChanges htmlNode domNode =
    findTextChangesRec htmlNode domNode []


findTextChangesRec : HtmlNode -> DomNode -> Path -> Result String (List TextChange)
findTextChangesRec htmlNode domNode backwardsNodePath =
    case domNode of
        DomNode domNodeContents ->
            case htmlNode of
                ElementNode tag _ children ->
                    let
                        domChildNodes =
                            Maybe.withDefault Array.empty domNodeContents.childNodes
                    in
                    if domNodeContents.nodeType /= domElementNodeType then
                        Err "Dom node is a text node, but I was expecting an element node"

                    else if Just (String.toUpper tag) /= domNodeContents.tagName then
                        Err <|
                            "Dom node's tag was "
                                ++ Maybe.withDefault "" domNodeContents.tagName
                                ++ ", but I was expecting "
                                ++ tag

                    else if Array.length domChildNodes /= Array.length children then
                        Err <|
                            "Dom node's children length was "
                                ++ (String.fromInt <| Array.length domChildNodes)
                                ++ ", but I was expecting "
                                ++ (String.fromInt <| Array.length children)

                    else
                        let
                            indexedNodePairs =
                                Array.indexedMap Tuple.pair <| Array.Extra.map2 Tuple.pair children domChildNodes
                        in
                        Array.foldl
                            (\( i, ( htmlChild, domChild ) ) resultTextChangeList ->
                                case resultTextChangeList of
                                    Err s ->
                                        Err s

                                    Ok x ->
                                        case findTextChangesRec htmlChild domChild (i :: backwardsNodePath) of
                                            Err s ->
                                                Err s

                                            Ok y ->
                                                Ok (x ++ y)
                            )
                            (Ok [])
                            indexedNodePairs

                TextNode textNodeText ->
                    if domNodeContents.nodeType /= domTextNodeType then
                        Err "Dom node was an element node, but I was expecting a text node"

                    else
                        case domNodeContents.nodeValue of
                            Nothing ->
                                Err "Dom node is a text node, but has no value"

                            Just domNodeText ->
                                let
                                    domNodeSanitizedText =
                                        if domNodeText == zeroWidthSpace then
                                            ""

                                        else
                                            domNodeText
                                in
                                if domNodeSanitizedText /= textNodeText then
                                    Ok [ ( List.reverse backwardsNodePath, domNodeSanitizedText ) ]

                                else
                                    Ok []
