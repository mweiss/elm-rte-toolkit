module Rte.List exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Annotation exposing (clearAnnotations)
import Rte.Commands exposing (altKey, backspaceKey, deleteKey, emptyCommandBinding, enterKey, inputEvent, isEmptyTextBlock, key, liftAnnotation, liftConcatMapFunc, otherwiseDo, returnKey, set)
import Rte.Marks exposing (ToggleAction(..), clearMarks, toggleMarkAtPath)
import Rte.Model exposing (ChildNodes(..), Command, CommandMap, EditorBlockNode, EditorInlineLeaf(..), ElementParameters, NodePath, Selection, elementParameters)
import Rte.Node exposing (EditorFragment(..), EditorNode(..), concatMap, findAncestor, findLastPath, joinBlocks, nodeAt, replace, replaceWithFragment)
import Rte.NodePath exposing (commonAncestor, decrement, increment)
import Rte.Selection exposing (clearSelectionAnnotations, isCollapsed, markSelection, normalizeSelection, selectionFromMarks)
import Set


type ListType
    = Ordered
    | Unordered


type alias ListDefinition =
    { ordered : ElementParameters, unordered : ElementParameters, item : ElementParameters }


commandBindings : ListDefinition -> CommandMap
commandBindings definition =
    let
        backspaceCommand =
            joinBackward definition

        deleteCommand =
            joinForward definition
    in
    emptyCommandBinding
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ] (liftEmpty definition |> otherwiseDo (split definition))
        |> set [ inputEvent "deleteContentBackward", key [ backspaceKey ] ] backspaceCommand
        |> set [ inputEvent "deleteContentForward", key [ deleteKey ] ] deleteCommand
        |> set [ inputEvent "deleteWordBackward", key [ altKey, backspaceKey ] ] backspaceCommand
        |> set [ inputEvent "deleteWordForward", key [ altKey, deleteKey ] ] deleteCommand


defaultListDefinition : ListDefinition
defaultListDefinition =
    { ordered = elementParameters "ol" [] Set.empty
    , unordered = elementParameters "ul" [] Set.empty
    , item = elementParameters "li" [] Set.empty
    }


addListItem : ListDefinition -> EditorBlockNode -> EditorBlockNode
addListItem definition node =
    { parameters = definition.item
    , childNodes = BlockArray <| Array.fromList [ node ]
    }


wrap : ListDefinition -> ListType -> Command
wrap definition type_ editorState =
    Rte.Commands.wrap (addListItem definition)
        (if type_ == Ordered then
            definition.ordered

         else
            definition.unordered
        )
        editorState


findListItemAncestor : ElementParameters -> NodePath -> EditorBlockNode -> Maybe ( NodePath, EditorBlockNode )
findListItemAncestor parameters =
    findAncestor (\n -> n.parameters.name == parameters.name)


split : ListDefinition -> Command
split definition =
    Rte.Commands.splitBlock (findListItemAncestor definition.item)


isListNode : ListDefinition -> EditorNode -> Bool
isListNode definition node =
    case node of
        InlineLeafWrapper _ ->
            False

        BlockNodeWrapper bn ->
            bn.parameters.name
                == definition.ordered.name
                || bn.parameters.name
                == definition.unordered.name


addLiftAnnotationAtPathAndChildren : NodePath -> EditorBlockNode -> Result String EditorBlockNode
addLiftAnnotationAtPathAndChildren path root =
    case Rte.Annotation.addAnnotationAtPath liftAnnotation path root of
        Err s ->
            Err s

        Ok newRoot ->
            case nodeAt path newRoot of
                Nothing ->
                    Err "Invalid path"

                Just node ->
                    case node of
                        BlockNodeWrapper bn ->
                            case bn.childNodes of
                                BlockArray ba ->
                                    List.foldl
                                        (\i result ->
                                            case result of
                                                Err _ ->
                                                    result

                                                Ok n ->
                                                    Rte.Annotation.addAnnotationAtPath liftAnnotation (path ++ [ i ]) n
                                        )
                                        (Ok newRoot)
                                        (List.range 0 (Array.length ba - 1))

                                _ ->
                                    Err "I was expecting a block array to add a lift mark to"

                        _ ->
                            Err "I was expecting a block node to add a lift mark to"


addLiftMarkToListItems : ListDefinition -> Selection -> EditorBlockNode -> Result String EditorBlockNode
addLiftMarkToListItems definition selection root =
    case findListItemAncestor definition.item selection.anchorNode root of
        Nothing ->
            Err "There is no list item ancestor at anchor path"

        Just ( start, _ ) ->
            case findListItemAncestor definition.item selection.focusNode root of
                Nothing ->
                    Err "There is no list item ancestor at focus path"

                Just ( end, _ ) ->
                    if start == end then
                        addLiftAnnotationAtPathAndChildren start root

                    else
                        let
                            ancestor =
                                commonAncestor start end
                        in
                        case nodeAt ancestor root of
                            Nothing ->
                                Err "Invalid ancestor path"

                            Just ancestorNode ->
                                if not <| isListNode definition ancestorNode then
                                    Err "I cannot lift list items unless the common ancestor is a list"

                                else
                                    case List.Extra.getAt (List.length ancestor) start of
                                        Nothing ->
                                            Err "Invalid start index"

                                        Just startIndex ->
                                            case List.Extra.getAt (List.length ancestor) end of
                                                Nothing ->
                                                    Err "Invalid end index"

                                                Just endIndex ->
                                                    List.foldl
                                                        (\i result ->
                                                            case result of
                                                                Err _ ->
                                                                    result

                                                                Ok node ->
                                                                    addLiftAnnotationAtPathAndChildren (ancestor ++ [ i ]) node
                                                        )
                                                        (Ok root)
                                                        (List.range startIndex endIndex)


lift : ListDefinition -> Command
lift definition editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalizeSelection selection
            in
            case addLiftMarkToListItems definition normalizedSelection <| markSelection normalizedSelection editorState.root of
                Err s ->
                    Err s

                Ok markedRoot ->
                    let
                        -- TODO: this logic looks suspicious...
                        liftedRoot =
                            concatMap liftConcatMapFunc <| concatMap liftConcatMapFunc markedRoot

                        newSelection =
                            selectionFromMarks liftedRoot normalizedSelection.anchorOffset normalizedSelection.focusOffset
                    in
                    Ok
                        { editorState
                            | selection = newSelection
                            , root = clearAnnotations liftAnnotation <| clearSelectionAnnotations liftedRoot
                        }


liftEmpty : ListDefinition -> Command
liftEmpty definition editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if (not <| isCollapsed selection) || selection.anchorOffset /= 0 then
                Err "I can only lift collapsed selections at the beginning of a text node"

            else
                case findListItemAncestor definition.item selection.anchorNode editorState.root of
                    Nothing ->
                        Err "No list item ancestor to lift"

                    Just ( _, node ) ->
                        case node.childNodes of
                            BlockArray a ->
                                case Array.get 0 a of
                                    Nothing ->
                                        Err "Cannot lift a list item with no children"

                                    Just firstNode ->
                                        if not <| isEmptyTextBlock (BlockNodeWrapper firstNode) then
                                            Err "I cannot lift a node that is not an empty text block"

                                        else
                                            lift definition editorState

                            _ ->
                                Err "I was expecting a list item to have block child nodes"


isBeginningOfListItem : ListDefinition -> Selection -> EditorBlockNode -> Bool
isBeginningOfListItem definition selection root =
    if not <| isCollapsed selection then
        False

    else if selection.anchorOffset /= 0 then
        False

    else
        case findListItemAncestor definition.item selection.anchorNode root of
            Nothing ->
                False

            Just ( p, _ ) ->
                let
                    relativePath =
                        List.drop (List.length p) selection.anchorNode
                in
                List.all (\i -> i == 0) relativePath


joinBackward : ListDefinition -> Command
joinBackward definition editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isBeginningOfListItem definition selection editorState.root then
                Err "I can only join a list item backward if the selection is the beginning of a list item"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection

                    markedRoot =
                        markSelection normalizedSelection editorState.root
                in
                case findListItemAncestor definition.item selection.anchorNode markedRoot of
                    Nothing ->
                        Err "There is no list item selected"

                    Just ( liPath, liNode ) ->
                        -- If this is the first list item in the list, we should do lift logic
                        if List.Extra.last liPath == Just 0 then
                            lift definition editorState

                        else
                            let
                                prevLiPath =
                                    decrement liPath
                            in
                            case nodeAt prevLiPath markedRoot of
                                Nothing ->
                                    Err "Invalid list item path"

                                Just prevLiNode ->
                                    case prevLiNode of
                                        InlineLeafWrapper _ ->
                                            Err "There is no list item at path"

                                        BlockNodeWrapper prevBn ->
                                            case joinBlocks prevBn liNode of
                                                Nothing ->
                                                    Err "Could not join list items"

                                                Just joinedLi ->
                                                    let
                                                        joinedNodes =
                                                            replace prevLiPath (BlockNodeWrapper joinedLi) markedRoot
                                                                |> Result.andThen
                                                                    (replaceWithFragment liPath (BlockNodeFragment Array.empty))
                                                    in
                                                    case joinedNodes of
                                                        Err s ->
                                                            Err s

                                                        Ok newRoot ->
                                                            Ok
                                                                { editorState
                                                                    | selection = selectionFromMarks newRoot selection.anchorOffset selection.focusOffset
                                                                    , root = clearSelectionAnnotations newRoot
                                                                }


isEndOfListItem : ListDefinition -> Selection -> EditorBlockNode -> Bool
isEndOfListItem definition selection root =
    if not <| isCollapsed selection then
        False

    else
        case findListItemAncestor definition.item selection.anchorNode root of
            Nothing ->
                False

            Just ( path, node ) ->
                let
                    ( lastPath, lastNode ) =
                        findLastPath node
                in
                if selection.anchorNode /= path ++ lastPath then
                    False

                else
                    case lastNode of
                        InlineLeafWrapper il ->
                            case il of
                                TextLeaf tl ->
                                    String.length tl.text == selection.anchorOffset

                                _ ->
                                    True

                        _ ->
                            True


joinForward : ListDefinition -> Command
joinForward definition editorState =
    case editorState.selection of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isEndOfListItem definition selection editorState.root then
                Err "I can only join a list item forward if the selection is at the end of a list item"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection

                    markedRoot =
                        markSelection normalizedSelection editorState.root
                in
                case findListItemAncestor definition.item selection.anchorNode markedRoot of
                    Nothing ->
                        Err "There is no list item selected"

                    Just ( liPath, liNode ) ->
                        let
                            nextLiPath =
                                increment liPath
                        in
                        case nodeAt nextLiPath markedRoot of
                            Nothing ->
                                Err "I cannot join forward a list item if there is no subsequent list item"

                            Just nextLi ->
                                case nextLi of
                                    InlineLeafWrapper _ ->
                                        Err "There is no list item at path"

                                    BlockNodeWrapper nextBn ->
                                        case joinBlocks liNode nextBn of
                                            Nothing ->
                                                Err "I could not join these list items"

                                            Just joinedLi ->
                                                let
                                                    joinedNodes =
                                                        replace liPath (BlockNodeWrapper joinedLi) markedRoot
                                                            |> Result.andThen
                                                                (replaceWithFragment nextLiPath (BlockNodeFragment Array.empty))
                                                in
                                                case joinedNodes of
                                                    Err s ->
                                                        Err s

                                                    Ok newRoot ->
                                                        Ok
                                                            { editorState
                                                                | selection = selectionFromMarks newRoot selection.anchorOffset selection.focusOffset
                                                                , root = clearSelectionAnnotations newRoot
                                                            }
