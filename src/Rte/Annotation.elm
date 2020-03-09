module Rte.Annotation exposing (..)

import Rte.Model
    exposing
        ( Annotation
        , EditorBlockNode
        , EditorInlineLeaf(..)
        , EditorNode(..)
        , NodePath
        )
import Rte.Node exposing (indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


addAnnotationAtPath : Annotation -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
addAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


removeAnnotationAtPath : Annotation -> NodePath -> EditorBlockNode -> Result String EditorBlockNode
removeAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (remove annotation n) node


removeAnnotationToSet : Annotation -> Set Annotation -> Set Annotation
removeAnnotationToSet =
    Set.remove


addAnnotationToSet : Annotation -> Set Annotation -> Set Annotation
addAnnotationToSet =
    Set.insert


remove : Annotation -> EditorNode -> EditorNode
remove =
    toggle removeAnnotationToSet


add : Annotation -> EditorNode -> EditorNode
add =
    toggle addAnnotationToSet


toggle : (Annotation -> Set Annotation -> Set Annotation) -> Annotation -> EditorNode -> EditorNode
toggle func annotation node =
    case node of
        BlockNodeWrapper bn ->
            let
                p =
                    bn.parameters
            in
            BlockNodeWrapper { bn | parameters = { p | annotations = func annotation bn.parameters.annotations } }

        InlineLeafWrapper il ->
            InlineLeafWrapper <|
                case il of
                    InlineLeaf l ->
                        let
                            p =
                                l.parameters
                        in
                        InlineLeaf { l | parameters = { p | annotations = func annotation l.parameters.annotations } }

                    TextLeaf tl ->
                        TextLeaf { tl | annotations = func annotation tl.annotations }


clearAnnotations : Annotation -> EditorBlockNode -> EditorBlockNode
clearAnnotations annotation root =
    case map (remove annotation) (BlockNodeWrapper root) of
        BlockNodeWrapper bn ->
            bn

        _ ->
            root


getAnnotationsFromNode : EditorNode -> Set Annotation
getAnnotationsFromNode node =
    case node of
        BlockNodeWrapper blockNode ->
            blockNode.parameters.annotations

        InlineLeafWrapper inlineLeaf ->
            case inlineLeaf of
                InlineLeaf p ->
                    p.parameters.annotations

                TextLeaf p ->
                    p.annotations


findPathsWithAnnotation : Annotation -> EditorBlockNode -> List NodePath
findPathsWithAnnotation annotation node =
    indexedFoldl
        (\path n agg ->
            if Set.member annotation <| getAnnotationsFromNode n then
                path :: agg

            else
                agg
        )
        []
        (BlockNodeWrapper node)
