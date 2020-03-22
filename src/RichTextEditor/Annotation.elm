module RichTextEditor.Annotation exposing
    ( add
    , addAnnotationAtPath
    , clearAnnotations
    , findPathsWithAnnotation
    , getAnnotationsFromNode
    , remove
    , removeAnnotationAtPath
    , toggle
    , toggleElementParameters
    )

import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , InlineLeaf(..)
        , Path
        , annotationsFromTextLeafParameters
        , blockNodeWithElement
        , elementFromBlockNode
        , elementFromInlineLeafParameters
        , inlineLeafParametersWithElement
        , textLeafParametersWithAnnotations
        )
import RichTextEditor.Node exposing (Node(..), indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


addAnnotationAtPath : String -> Path -> BlockNode -> Result String BlockNode
addAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


removeAnnotationAtPath : String -> Path -> BlockNode -> Result String BlockNode
removeAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (remove annotation n) node


remove : String -> Node -> Node
remove =
    toggle Set.remove


add : String -> Node -> Node
add =
    toggle Set.insert


toggleElementParameters : (String -> Set String -> Set String) -> String -> Element -> Element
toggleElementParameters func annotation parameters =
    let
        annotations =
            Element.annotations parameters
    in
    Element.withAnnotations (func annotation annotations) parameters


toggle : (String -> Set String -> Set String) -> String -> Node -> Node
toggle func annotation node =
    case node of
        Block bn ->
            let
                newParameters =
                    toggleElementParameters func annotation (elementFromBlockNode bn)

                newBlockNode =
                    bn |> blockNodeWithElement newParameters
            in
            Block newBlockNode

        Inline il ->
            Inline <|
                case il of
                    InlineLeaf l ->
                        let
                            newParameters =
                                toggleElementParameters func annotation (elementFromInlineLeafParameters l)
                        in
                        InlineLeaf <| inlineLeafParametersWithElement newParameters l

                    TextLeaf tl ->
                        TextLeaf <| (tl |> textLeafParametersWithAnnotations (func annotation <| annotationsFromTextLeafParameters tl))


clearAnnotations : String -> BlockNode -> BlockNode
clearAnnotations annotation root =
    case map (remove annotation) (Block root) of
        Block bn ->
            bn

        _ ->
            root


getAnnotationsFromNode : Node -> Set String
getAnnotationsFromNode node =
    case node of
        Block blockNode ->
            Element.annotations <| elementFromBlockNode blockNode

        Inline inlineLeaf ->
            case inlineLeaf of
                InlineLeaf p ->
                    Element.annotations <| elementFromInlineLeafParameters p

                TextLeaf p ->
                    annotationsFromTextLeafParameters p


findPathsWithAnnotation : String -> BlockNode -> List Path
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
