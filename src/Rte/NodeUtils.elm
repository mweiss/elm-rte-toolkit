module Rte.NodeUtils exposing (..)

import Array
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf, NodePath)


type NodeResult
    = BlockNodeResult EditorBlockNode
    | InlineLeafResult EditorInlineLeaf
    | NoResult


{-| findNode returns the node at the specified NodePath if it exists.
-}
findNode : NodePath -> EditorBlockNode -> NodeResult
findNode path node =
    case path of
        [] ->
            BlockNodeResult node

        x :: xs ->
            case node.childNodes of
                BlockArray list ->
                    case Array.get x list of
                        Nothing ->
                            NoResult

                        Just childNode ->
                            findNode xs childNode

                InlineLeafArray list ->
                    case Array.get x list of
                        Nothing ->
                            NoResult

                        Just childLeafNode ->
                            if List.isEmpty xs then
                                InlineLeafResult childLeafNode

                            else
                                NoResult

                Leaf ->
                    NoResult
