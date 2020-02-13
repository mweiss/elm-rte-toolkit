module Rte.NodeUtils exposing (..)

import List.Extra
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf, NodePath)


type NodeResult
    = BlockNodeResult EditorBlockNode
    | InlineLeafResult EditorInlineLeaf
    | NoResult


findNode : NodePath -> EditorBlockNode -> NodeResult
findNode path node =
    case path of
        [] ->
            BlockNodeResult node

        x :: xs ->
            case node.childNodes of
                BlockList list ->
                    case List.Extra.getAt x list of
                        Nothing ->
                            NoResult

                        Just childNode ->
                            findNode xs childNode

                InlineLeafList list ->
                    case List.Extra.getAt x list of
                        Nothing ->
                            NoResult

                        Just childLeafNode ->
                            if List.isEmpty xs then
                                InlineLeafResult childLeafNode

                            else
                                NoResult

                Leaf ->
                    NoResult
