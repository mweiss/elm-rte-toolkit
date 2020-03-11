module RichTextEditor.Annotation exposing (..)

import RichTextEditor.Model.Annotation exposing (Annotation)
import RichTextEditor.Model.Node exposing (BlockNode, ElementParameters, InlineLeaf(..), Node(..), Path, annotationsFromElementParameters, annotationsFromTextLeafParameters, blockNodeWithElementParameters, elementParametersFromBlockNode, elementParametersFromInlineLeafParameters, elementParametersWithAnnotations, inlineLeafParametersWithElementParameters, textLeafParametersWithAnnotations)
import RichTextEditor.Node exposing (indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


addAnnotationAtPath : Annotation -> Path -> BlockNode -> Result String BlockNode
addAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


removeAnnotationAtPath : Annotation -> Path -> BlockNode -> Result String BlockNode
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


remove : Annotation -> Node -> Node
remove =
    toggle removeAnnotationToSet


add : Annotation -> Node -> Node
add =
    toggle addAnnotationToSet


toggleElementParameters : (Annotation -> Set Annotation -> Set Annotation) -> Annotation -> ElementParameters -> ElementParameters
toggleElementParameters func annotation parameters =
    let
        annotations =
            annotationsFromElementParameters parameters
    in
    elementParametersWithAnnotations (func annotation annotations) parameters


toggle : (Annotation -> Set Annotation -> Set Annotation) -> Annotation -> Node -> Node
toggle func annotation node =
    case node of
        Block bn ->
            let
                newParameters =
                    toggleElementParameters func annotation (elementParametersFromBlockNode bn)

                newBlockNode =
                    bn |> blockNodeWithElementParameters newParameters
            in
            Block newBlockNode

        Inline il ->
            Inline <|
                case il of
                    InlineLeaf l ->
                        let
                            newParameters =
                                toggleElementParameters func annotation (elementParametersFromInlineLeafParameters l)
                        in
                        InlineLeaf <| inlineLeafParametersWithElementParameters newParameters l

                    TextLeaf tl ->
                        TextLeaf <| (tl |> textLeafParametersWithAnnotations (func annotation <| annotationsFromTextLeafParameters tl))


clearAnnotations : Annotation -> BlockNode -> BlockNode
clearAnnotations annotation root =
    case map (remove annotation) (Block root) of
        Block bn ->
            bn

        _ ->
            root


getAnnotationsFromNode : Node -> Set Annotation
getAnnotationsFromNode node =
    case node of
        Block blockNode ->
            annotationsFromElementParameters <| elementParametersFromBlockNode blockNode

        Inline inlineLeaf ->
            case inlineLeaf of
                InlineLeaf p ->
                    annotationsFromElementParameters <| elementParametersFromInlineLeafParameters p

                TextLeaf p ->
                    annotationsFromTextLeafParameters p


findPathsWithAnnotation : Annotation -> BlockNode -> List Path
findPathsWithAnnotation annotation node =
    indexedFoldl
        (\path n agg ->
            if Set.member annotation <| getAnnotationsFromNode n then
                path :: agg

            else
                agg
        )
        []
        (Block node)
