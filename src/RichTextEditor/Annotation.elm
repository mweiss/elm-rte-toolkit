module RichTextEditor.Annotation exposing
    ( selection, selectable, lift
    , add, addAnnotationAtPath, annotateSelection, annotationsFromNode, clearAnnotations, clearSelectionAnnotations, findPathsWithAnnotation, remove, removeAnnotationAtPath, selectionFromAnnotations, toggle, toggleElementParameters
    )

{-| This module contains common constants used as node annotations. Annotations can be added to
elements and text to keep track of position when doing a complex transform like a lift or join,
as well as add flags to a node that you can use to effect behavior, like if something is selectable.

    newElement =
        element |> Element.withAnnotations (Set.singleton selection)

@docs selection, selectable, lift

-}

import RichTextEditor.Internal.Constants as Constants
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.InlineElement as InlineElement
import RichTextEditor.Model.Node
    exposing
        ( Block
        , Inline(..)
        , Path
        , element
        , withElement
        )
import RichTextEditor.Model.Selection exposing (Selection, anchorNode, focusNode, range)
import RichTextEditor.Model.Text as Text
import RichTextEditor.Node exposing (Node(..), indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


{-| Represents that a node is currently selected. This annotation is transient, e.g. it
should be cleared before a transform or command is complete. This annotation is also used when
rendering to annotate a selected node for decorators.
-}
selection : String
selection =
    Constants.selection


{-| Represents that a node is can be selected. This annotation is not transient.
-}
selectable : String
selectable =
    Constants.selectable


{-| Represents that a node is can be selected. This annotation is transient, e.g. it should be
cleared before a transform or command is complete.
-}
lift : String
lift =
    Constants.lift


addAnnotationAtPath : String -> Path -> Block -> Result String Block
addAnnotationAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


removeAnnotationAtPath : String -> Path -> Block -> Result String Block
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
                    toggleElementParameters func annotation (element bn)

                newBlockNode =
                    bn |> withElement newParameters
            in
            Block newBlockNode

        Inline il ->
            Inline <|
                case il of
                    InlineElement l ->
                        let
                            newParameters =
                                toggleElementParameters func annotation (InlineElement.element l)
                        in
                        InlineElement <| InlineElement.withElement newParameters l

                    Text tl ->
                        Text <| (tl |> Text.withAnnotations (func annotation <| Text.annotations tl))


clearAnnotations : String -> Block -> Block
clearAnnotations annotation root =
    case map (remove annotation) (Block root) of
        Block bn ->
            bn

        _ ->
            root


annotationsFromNode : Node -> Set String
annotationsFromNode node =
    case node of
        Block blockNode ->
            Element.annotations <| element blockNode

        Inline inlineLeaf ->
            case inlineLeaf of
                InlineElement p ->
                    Element.annotations <| InlineElement.element p

                Text p ->
                    Text.annotations p


findPathsWithAnnotation : String -> Block -> List Path
findPathsWithAnnotation annotation node =
    indexedFoldl
        (\path n agg ->
            if Set.member annotation <| annotationsFromNode n then
                path :: agg

            else
                agg
        )
        []
        (Block node)


annotateSelection : Selection -> Block -> Block
annotateSelection selection_ node =
    addSelectionAnnotationAtPath (focusNode selection_) <| addSelectionAnnotationAtPath (anchorNode selection_) node


addSelectionAnnotationAtPath : Path -> Block -> Block
addSelectionAnnotationAtPath nodePath node =
    Result.withDefault node (addAnnotationAtPath selection nodePath node)


clearSelectionAnnotations : Block -> Block
clearSelectionAnnotations =
    clearAnnotations selection


selectionFromAnnotations : Block -> Int -> Int -> Maybe Selection
selectionFromAnnotations node anchorOffset focusOffset =
    case findNodeRangeFromSelectionAnnotations node of
        Nothing ->
            Nothing

        Just ( start, end ) ->
            Just (range start anchorOffset end focusOffset)


findNodeRangeFromSelectionAnnotations : Block -> Maybe ( Path, Path )
findNodeRangeFromSelectionAnnotations node =
    let
        paths =
            findPathsWithAnnotation selection node
    in
    case paths of
        [] ->
            Nothing

        [ x ] ->
            Just ( x, x )

        end :: start :: _ ->
            Just ( start, end )
