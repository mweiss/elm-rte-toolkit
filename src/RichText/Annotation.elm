module RichText.Annotation exposing
    ( selection, selectable, lift
    , add, addAtPath, addToBlock, addToInline, fromNode, clear, remove, removeAtPath, removeFromBlock, removeFromInline
    , annotateSelection, selectionFromAnnotations, clearSelectionAnnotations, isSelectable
    , doLift
    )

{-| This module contains common constants and functions used to annotate nodes.
Annotations can be added to elements and text to keep track of position when doing a complex
transform like a lift or join, as well as add flags to a node that you can use to effect behavior,
like if something is selectable.

    newElement =
        element |> Element.withAnnotations (Set.singleton selection)


# Annotations

@docs selection, selectable, lift


# Helpers

@docs add, addAtPath, addToBlock, addToInline, fromNode, clear, remove, removeAtPath, removeFromBlock, removeFromInline


# Selection

These methods are for marking selection, which is useful for keeping track of a user's selection
when defining your own transforms.

@docs annotateSelection, selectionFromAnnotations, clearSelectionAnnotations, isSelectable


# Lift

@docs doLift

-}

import Array
import List.Extra
import RichText.Internal.Constants as Constants
import RichText.Model.Element as Element exposing (Element)
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , Path
        , blockChildren
        , childNodes
        , element
        , toBlockArray
        , withChildNodes
        , withElement
        )
import RichText.Model.Selection exposing (Selection, anchorNode, focusNode, range)
import RichText.Model.Text as Text
import RichText.Node exposing (Node(..), concatMap, indexedFoldl, map, nodeAt, replace)
import Set exposing (Set)


{-| Represents that a node is currently selected. This annotation is transient, e.g. it
should be cleared before a transform or command is complete. This annotation is also used when
rendering to annotate a selected node for decorators.
-}
selection : String
selection =
    Constants.selection


{-| Represents that a node can be selected. This annotation is not transient.
-}
selectable : String
selectable =
    Constants.selectable


{-| Represents that a node should be lifted. This annotation is transient, e.g. it should be
cleared before a transform or command is complete.
-}
lift : String
lift =
    Constants.lift


{-| Adds an annotation to the node at the given path. Returns an error if no node
exists at that path.

    Annotation.addAtPath "myAnnotation" path root

-}
addAtPath : String -> Path -> Block -> Result String Block
addAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (add annotation n) node


{-| Removes the given annotation to the node at the given path. Returns an error if no node
exists at that path.

    Annotation.removeAtPath "myAnnotation" path root

-}
removeAtPath : String -> Path -> Block -> Result String Block
removeAtPath annotation path node =
    case nodeAt path node of
        Nothing ->
            Err "No block found at path"

        Just n ->
            replace path (remove annotation n) node


{-| Removes the given annotation from the node if it exists.

    remove Annotation.selectable (Block horizontal_rule)
    --> Returns (Block horizontal_rule) with the selectable annotation removed

-}
remove : String -> Node -> Node
remove =
    toggle Set.remove


{-| Adds the given annotation to the node.

    add Annotation.selectable (Block horizontal_rule)
    --> Returns (Block horizontal_rule) with the selectable annotation added

-}
add : String -> Node -> Node
add =
    toggle Set.insert


{-| Helper which adds the given annotation to a block node.
-}
addToBlock : String -> Block -> Block
addToBlock a n =
    case add a (Block n) of
        Block b ->
            b

        _ ->
            n


{-| Helper which adds the given annotation to an inline node.
-}
addToInline : String -> Inline -> Inline
addToInline a n =
    case add a (Inline n) of
        Inline i ->
            i

        _ ->
            n


{-| Helper which removes the given annotation from a block node.
-}
removeFromBlock : String -> Block -> Block
removeFromBlock a n =
    case remove a (Block n) of
        Block b ->
            b

        _ ->
            n


{-| Helper which removes the given annotation from an inline node.
-}
removeFromInline : String -> Inline -> Inline
removeFromInline a n =
    case remove a (Inline n) of
        Inline i ->
            i

        _ ->
            n


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


{-| Removes the given annotation this node and its children.

    clear Annotation.lift root
    --> Returns `root` but with all the lift annotations removed.

-}
clear : String -> Block -> Block
clear annotation root =
    case map (remove annotation) (Block root) of
        Block bn ->
            bn

        _ ->
            root


{-| Helper method to extract annotations from a node.

    fromNode node
    --> Set ["__selectable__"]

-}
fromNode : Node -> Set String
fromNode node =
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
            if Set.member annotation <| fromNode n then
                path :: agg

            else
                agg
        )
        []
        (Block node)


{-| Adds the selection annotation to the paths in the selection if they exist. This is useful
when defining your own transforms to keep track of which nodes are selected.

    markedRoot =
        annotateSelection normalizedSelection (State.root editorState)

-}
annotateSelection : Selection -> Block -> Block
annotateSelection selection_ node =
    addSelectionAnnotationAtPath (focusNode selection_) <| addSelectionAnnotationAtPath (anchorNode selection_) node


addSelectionAnnotationAtPath : Path -> Block -> Block
addSelectionAnnotationAtPath nodePath node =
    Result.withDefault node (addAtPath selection nodePath node)


{-| Clears the selection annotation from the editor node. The selection annotation should be
transient, so it's important to clear the annotation once you're finished with it.

    clearSelectionAnnotations root
    --> Returns root but with the selection annotation removed

-}
clearSelectionAnnotations : Block -> Block
clearSelectionAnnotations =
    clear selection


{-| Derives the selection from selection annotations.

    selectionFromAnnotations root 0 0
    --> Just { anchorNode=[0], anchorOffset=0, focusNode=[1,2], focusOffset=0 }

-}
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


{-| True if a node has the `selectable` annotation or is Text, false otherwise.

    isSelectable (Inline textNode)
    --> True

-}
isSelectable : Node -> Bool
isSelectable node =
    case node of
        Block bn ->
            Set.member selectable (Element.annotations (element bn))

        Inline ln ->
            case ln of
                Text _ ->
                    True

                InlineElement l ->
                    Set.member selectable (Element.annotations (InlineElement.element l))


annotationsFromBlockNode : Block -> Set String
annotationsFromBlockNode node =
    Element.annotations <| Node.element node


liftConcatMapFunc : Node -> List Node
liftConcatMapFunc node =
    case node of
        Block bn ->
            case childNodes bn of
                Leaf ->
                    [ node ]

                InlineChildren _ ->
                    [ node ]

                BlockChildren a ->
                    let
                        groupedBlockNodes =
                            List.Extra.groupWhile
                                (\n1 n2 ->
                                    Set.member
                                        lift
                                        (annotationsFromBlockNode n1)
                                        == Set.member
                                            lift
                                            (annotationsFromBlockNode n2)
                                )
                                (Array.toList (toBlockArray a))
                    in
                    List.map Block <|
                        List.concatMap
                            (\( n, l ) ->
                                if Set.member lift (annotationsFromBlockNode n) then
                                    n :: l

                                else
                                    [ bn |> withChildNodes (blockChildren (Array.fromList <| n :: l)) ]
                            )
                            groupedBlockNodes

        Inline _ ->
            [ node ]


{-| Lifts nodes that are marked with the lift annotation if possible. Note that if there are multiple
levels of lift annotations, you may have to call this function multiple times.

    markedRoot : Block
    markedRoot =
        addLiftMarkToBlocksInSelection normalizedSelection root

    liftedRoot : Block
    liftedRoot =
        doLift markedRoot

-}
doLift : Block -> Block
doLift root =
    concatMap liftConcatMapFunc root
