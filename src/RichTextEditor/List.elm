module RichTextEditor.List exposing (..)

import Array exposing (Array)
import List.Extra
import RichTextEditor.Annotation exposing (clearAnnotations)
import RichTextEditor.Commands
    exposing
        ( isEmptyTextBlock
        , liftAnnotation
        , liftConcatMapFunc
        )
import RichTextEditor.Model.Command exposing (CommandMap, Transform, emptyCommandMap, inputEvent, key, set, transformCommand)
import RichTextEditor.Model.Keys exposing (altKey, backspaceKey, deleteKey, enterKey, returnKey)
import RichTextEditor.Model.Node exposing (BlockArray, BlockNode, ChildNodes(..), EditorInlineLeaf(..), ElementParameters, Fragment(..), Node(..), Path, blockArray, blockNode, childNodes, elementParameters, elementParametersFromBlockNode, fromBlockArray, nameFromElementParameters, text)
import RichTextEditor.Model.Selection exposing (Selection, anchorNode, anchorOffset, focusNode, focusOffset, isCollapsed, normalizeSelection)
import RichTextEditor.Model.State as State exposing (root, withRoot, withSelection)
import RichTextEditor.Node
    exposing
        ( concatMap
        , findAncestor
        , findLastPath
        , joinBlocks
        , nodeAt
        , replace
        , replaceWithFragment
        )
import RichTextEditor.NodePath exposing (commonAncestor, decrement, increment)
import RichTextEditor.Selection
    exposing
        ( annotateSelection
        , clearSelectionAnnotations
        , selectionFromAnnotations
        )
import Set


type ListType
    = Ordered
    | Unordered


type ListDefinition
    = ListDefinition ListDefinitionContents


type alias ListDefinitionContents =
    { ordered : ElementParameters, unordered : ElementParameters, item : ElementParameters }


commandBindings : ListDefinition -> CommandMap
commandBindings definition =
    let
        backspaceCommand =
            joinBackward definition

        deleteCommand =
            joinForward definition
    in
    emptyCommandMap
        |> set [ inputEvent "insertParagraph", key [ enterKey ], key [ returnKey ] ]
            [ ( "liftEmptyListItem", transformCommand <| liftEmpty definition )
            , ( "splitListItem", transformCommand <| split definition )
            ]
        |> set [ inputEvent "deleteContentBackward", key [ backspaceKey ] ]
            [ ( "joinListBackward", transformCommand <| backspaceCommand ) ]
        |> set [ inputEvent "deleteContentForward", key [ deleteKey ] ]
            [ ( "joinListForward", transformCommand <| deleteCommand ) ]
        |> set [ inputEvent "deleteWordBackward", key [ altKey, backspaceKey ] ]
            [ ( "joinListBackward", transformCommand <| backspaceCommand ) ]
        |> set [ inputEvent "deleteWordForward", key [ altKey, deleteKey ] ]
            [ ( "joinListForward", transformCommand <| deleteCommand ) ]


defaultListDefinition : ListDefinition
defaultListDefinition =
    ListDefinition
        { ordered = elementParameters "ordered_list" [] Set.empty
        , unordered = elementParameters "unordered_list" [] Set.empty
        , item = elementParameters "list_item" [] Set.empty
        }


item : ListDefinition -> ElementParameters
item definition =
    case definition of
        ListDefinition c ->
            c.item


ordered : ListDefinition -> ElementParameters
ordered definition =
    case definition of
        ListDefinition c ->
            c.ordered


unordered : ListDefinition -> ElementParameters
unordered definition =
    case definition of
        ListDefinition c ->
            c.unordered


addListItem : ListDefinition -> BlockNode -> BlockNode
addListItem definition node =
    blockNode
        (item definition)
        (blockArray <|
            Array.fromList [ node ]
        )


wrap : ListDefinition -> ListType -> Transform
wrap definition type_ editorState =
    RichTextEditor.Commands.wrap (addListItem definition)
        (if type_ == Ordered then
            ordered definition

         else
            unordered definition
        )
        editorState


findListItemAncestor : ElementParameters -> Path -> BlockNode -> Maybe ( Path, BlockNode )
findListItemAncestor parameters =
    findAncestor (\n -> nameFromElementParameters (elementParametersFromBlockNode n) == nameFromElementParameters parameters)


split : ListDefinition -> Transform
split definition =
    RichTextEditor.Commands.splitBlock (findListItemAncestor (item definition))


isListNode : ListDefinition -> Node -> Bool
isListNode definition node =
    case node of
        InlineLeafWrapper _ ->
            False

        BlockNodeWrapper bn ->
            let
                bnName =
                    nameFromElementParameters (elementParametersFromBlockNode bn)
            in
            bnName
                == nameFromElementParameters (ordered definition)
                || bnName
                == nameFromElementParameters (unordered definition)


addLiftAnnotationAtPathAndChildren : Path -> BlockNode -> Result String BlockNode
addLiftAnnotationAtPathAndChildren path root =
    case RichTextEditor.Annotation.addAnnotationAtPath liftAnnotation path root of
        Err s ->
            Err s

        Ok newRoot ->
            case nodeAt path newRoot of
                Nothing ->
                    Err "Invalid path"

                Just node ->
                    case node of
                        BlockNodeWrapper bn ->
                            case childNodes bn of
                                BlockChildren ba ->
                                    List.foldl
                                        (\i result ->
                                            case result of
                                                Err _ ->
                                                    result

                                                Ok n ->
                                                    RichTextEditor.Annotation.addAnnotationAtPath liftAnnotation (path ++ [ i ]) n
                                        )
                                        (Ok newRoot)
                                        (List.range 0 (Array.length (fromBlockArray ba) - 1))

                                _ ->
                                    Err "I was expecting a block array to add a lift mark to"

                        _ ->
                            Err "I was expecting a block node to add a lift mark to"


addLiftMarkToListItems : ListDefinition -> Selection -> BlockNode -> Result String BlockNode
addLiftMarkToListItems definition selection root =
    case findListItemAncestor (item definition) (anchorNode selection) root of
        Nothing ->
            Err "There is no list item ancestor at anchor path"

        Just ( start, _ ) ->
            case findListItemAncestor (item definition) (focusNode selection) root of
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


lift : ListDefinition -> Transform
lift definition editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalizeSelection selection
            in
            case addLiftMarkToListItems definition normalizedSelection <| annotateSelection normalizedSelection (State.root editorState) of
                Err s ->
                    Err s

                Ok markedRoot ->
                    let
                        -- TODO: this logic looks suspicious...
                        liftedRoot =
                            concatMap liftConcatMapFunc <| concatMap liftConcatMapFunc markedRoot

                        newSelection =
                            selectionFromAnnotations liftedRoot (anchorOffset normalizedSelection) (focusOffset normalizedSelection)
                    in
                    Ok
                        (editorState
                            |> withSelection newSelection
                            |> withRoot (clearAnnotations liftAnnotation <| clearSelectionAnnotations liftedRoot)
                        )


liftEmpty : ListDefinition -> Transform
liftEmpty definition editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if (not <| isCollapsed selection) || anchorOffset selection /= 0 then
                Err "I can only lift collapsed selections at the beginning of a text node"

            else
                case findListItemAncestor (item definition) (anchorNode selection) (State.root editorState) of
                    Nothing ->
                        Err "No list item ancestor to lift"

                    Just ( _, node ) ->
                        case childNodes node of
                            BlockChildren a ->
                                case Array.get 0 (fromBlockArray a) of
                                    Nothing ->
                                        Err "Cannot lift a list item with no children"

                                    Just firstNode ->
                                        if not <| isEmptyTextBlock (BlockNodeWrapper firstNode) then
                                            Err "I cannot lift a node that is not an empty text block"

                                        else
                                            lift definition editorState

                            _ ->
                                Err "I was expecting a list item to have block child nodes"


isBeginningOfListItem : ListDefinition -> Selection -> BlockNode -> Bool
isBeginningOfListItem definition selection root =
    if not <| isCollapsed selection then
        False

    else if anchorOffset selection /= 0 then
        False

    else
        case findListItemAncestor (item definition) (anchorNode selection) root of
            Nothing ->
                False

            Just ( p, _ ) ->
                let
                    relativePath =
                        List.drop (List.length p) (anchorNode selection)
                in
                List.all (\i -> i == 0) relativePath


joinBackward : ListDefinition -> Transform
joinBackward definition editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isBeginningOfListItem definition selection (State.root editorState) then
                Err "I can only join a list item backward if the selection is the beginning of a list item"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection

                    markedRoot =
                        annotateSelection normalizedSelection (State.root editorState)
                in
                case findListItemAncestor (item definition) (anchorNode selection) markedRoot of
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
                                                                (editorState
                                                                    |> withSelection
                                                                        (selectionFromAnnotations
                                                                            newRoot
                                                                            (anchorOffset selection)
                                                                            (focusOffset selection)
                                                                        )
                                                                    |> withRoot (clearSelectionAnnotations newRoot)
                                                                )


isEndOfListItem : ListDefinition -> Selection -> BlockNode -> Bool
isEndOfListItem definition selection root =
    if not <| isCollapsed selection then
        False

    else
        case findListItemAncestor (item definition) (anchorNode selection) root of
            Nothing ->
                False

            Just ( path, node ) ->
                let
                    ( lastPath, lastNode ) =
                        findLastPath node
                in
                if anchorNode selection /= path ++ lastPath then
                    False

                else
                    case lastNode of
                        InlineLeafWrapper il ->
                            case il of
                                TextLeaf tl ->
                                    String.length (text tl) == anchorOffset selection

                                _ ->
                                    True

                        _ ->
                            True


joinForward : ListDefinition -> Transform
joinForward definition editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            if not <| isEndOfListItem definition selection (State.root editorState) then
                Err "I can only join a list item forward if the selection is at the end of a list item"

            else
                let
                    normalizedSelection =
                        normalizeSelection selection

                    markedRoot =
                        annotateSelection normalizedSelection (State.root editorState)
                in
                case findListItemAncestor (item definition) (anchorNode selection) markedRoot of
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
                                                            (editorState
                                                                |> withSelection (selectionFromAnnotations newRoot (anchorOffset selection) (focusOffset selection))
                                                                |> withRoot (clearSelectionAnnotations newRoot)
                                                            )