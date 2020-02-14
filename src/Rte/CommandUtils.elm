module Rte.CommandUtils exposing (..)

import Rte.Model exposing (EditorBlockNode, EditorInlineLeaf(..), NodePath, Selection)
import Rte.NodePath exposing (toString)
import Rte.NodeUtils exposing (EditorNode(..), NodeResult(..), findNode, replaceNode)


removeTextAtRange : NodePath -> Int -> Maybe Int -> EditorBlockNode -> Result String EditorBlockNode
removeTextAtRange nodePath start maybeEnd root =
    case findNode nodePath root of
        InlineLeafResult leaf ->
            case leaf of
                InlineLeaf l ->
                    Err "I was expecting a text leaf, but instead I got an inline leaf"

                TextLeaf v ->
                    let
                        textNode =
                            case maybeEnd of
                                Nothing ->
                                    TextLeaf { v | text = String.left start v.text }

                                Just end ->
                                    TextLeaf { v | text = String.left start v.text ++ String.dropLeft end v.text }
                    in
                    replaceNode nodePath (InlineLeafWrapper textNode) root

        BlockNodeResult _ ->
            Err "I was expecting a text node, but instead I got a block node"

        NoResult ->
            Err <| "There is no node at node path " ++ toString nodePath
