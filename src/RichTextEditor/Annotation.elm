module RichTextEditor.Annotation exposing (..)

import RichTextEditor.Model.Annotation exposing (Annotation)
import RichTextEditor.Model.Node exposing (EditorBlockNode, EditorInlineLeaf(..), EditorNode(..), ElementParameters, NodePath, annotationsFromElementParameters, annotationsFromTextLeafParameters, blockNodeWithParameters, elementParametersFromBlockNode, elementParametersFromInlineLeafParameters, elementParametersWithAnnotations, inlineLeafParametersWithElementParameters, textLeafParametersWithAnnotations)
import RichTextEditor.Node exposing (indexedFoldl, map, nodeAt, replace)
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


toggleElementParameters : (Annotation -> Set Annotation -> Set Annotation) -> Annotation -> ElementParameters -> ElementParameters
toggleElementParameters func annotation parameters =
    let
        annotations =
            annotationsFromElementParameters parameters
    in
    elementParametersWithAnnotations (func annotation annotations) parameters


toggle : (Annotation -> Set Annotation -> Set Annotation) -> Annotation -> EditorNode -> EditorNode
toggle func annotation node =
    case node of
        BlockNodeWrapper bn ->
            let
                newParameters =
                    toggleElementParameters func annotation (elementParametersFromBlockNode bn)

                newBlockNode =
                    bn |> blockNodeWithParameters newParameters
            in
            BlockNodeWrapper newBlockNode

        InlineLeafWrapper il ->
            InlineLeafWrapper <|
                case il of
                    InlineLeaf l ->
                        let
                            newParameters =
                                toggleElementParameters func annotation (elementParametersFromInlineLeafParameters l)
                        in
                        InlineLeaf <| inlineLeafParametersWithElementParameters newParameters l

                    TextLeaf tl ->
                        TextLeaf <| tl |> textLeafParametersWithAnnotations (func annotation <| annotationsFromTextLeafParameters tl)


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
            annotationsFromElementParameters <| elementParametersFromBlockNode blockNode

        InlineLeafWrapper inlineLeaf ->
            case inlineLeaf of
                InlineLeaf p ->
                    annotationsFromElementParameters <| elementParametersFromInlineLeafParameters p

                TextLeaf p ->
                    annotationsFromTextLeafParameters p


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
