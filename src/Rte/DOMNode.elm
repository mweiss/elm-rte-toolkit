module Rte.DOMNode exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Rte.EditorUtils exposing (zeroWidthSpace)
import Rte.Model exposing (DOMNode(..), DOMNodeContents, HtmlNode(..), NodePath, TextChange)


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



-- TODO: fill this in


findTextChanges : HtmlNode -> DOMNode -> Maybe (List TextChange)
findTextChanges htmlNode domNode =
    findTextChangesRec htmlNode domNode []


findTextChangesRec : HtmlNode -> DOMNode -> NodePath -> Maybe (List TextChange)
findTextChangesRec htmlNode domNode backwardsNodePath =
    case domNode of
        DOMNode domNodeContents ->
            case htmlNode of
                ElementNode tag attributes children ->
                    let
                        domChildNodes =
                            Maybe.withDefault [] domNodeContents.childNodes
                    in
                    if
                        domNodeContents.nodeType
                            /= domElementNodeType
                            || Just (String.toUpper tag)
                            /= domNodeContents.tagName
                            || List.length domChildNodes
                            /= List.length children
                    then
                        Nothing

                    else
                        let
                            indexedNodePairs =
                                List.indexedMap Tuple.pair <| List.map2 Tuple.pair children domChildNodes
                        in
                        List.foldl
                            (\( i, ( htmlChild, domChild ) ) maybeTextChangeList ->
                                case maybeTextChangeList of
                                    Nothing ->
                                        Nothing

                                    Just x ->
                                        case findTextChangesRec htmlChild domChild (i :: backwardsNodePath) of
                                            Nothing ->
                                                Nothing

                                            Just y ->
                                                Just (x ++ y)
                            )
                            (Just [])
                            indexedNodePairs

                TextNode textNodeText ->
                    if domNodeContents.nodeType /= domTextNodeType then
                        Nothing

                    else
                        case domNodeContents.nodeValue of
                            Nothing ->
                                Nothing

                            Just domNodeText ->
                                let
                                    domNodeSanitizedText =
                                        if domNodeText == zeroWidthSpace then
                                            ""

                                        else
                                            domNodeText
                                in
                                if domNodeSanitizedText /= textNodeText then
                                    Just [ ( List.reverse backwardsNodePath, domNodeSanitizedText ) ]

                                else
                                    Just []
