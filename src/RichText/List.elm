module RichText.List exposing
    ( ListDefinition, ListType(..), listDefinition, defaultListDefinition, ordered, unordered, item
    , defaultCommandMap
    , joinBackward, joinForward, lift, liftEmpty, split, wrap
    , findListItemAncestor, isBeginningOfListItem, isEndOfListItem, isListNode
    )

{-| This module contains definitions, commands, and transforms related to lists.


# Types and definitions

@docs ListDefinition, ListType, listDefinition, defaultListDefinition, ordered, unordered, item


# Commands

@docs defaultCommandMap


## Transforms

@docs joinBackward, joinForward, lift, liftEmpty, split, wrap


# Helpers

@docs findListItemAncestor, isBeginningOfListItem, isEndOfListItem, isListNode

-}

import Array exposing (Array)
import List.Extra
import RichText.Annotation as Annotation
    exposing
        ( annotateSelection
        , clear
        , clearSelectionAnnotations
        , doLift
        , selectionFromAnnotations
        )
import RichText.Commands
import RichText.Config.Command
    exposing
        ( CommandMap
        , Transform
        , emptyCommandMap
        , inputEvent
        , key
        , set
        , transform
        )
import RichText.Config.Keys
    exposing
        ( alt
        , backspace
        , delete
        , enter
        , return
        )
import RichText.Definitions exposing (listItem, orderedList, unorderedList)
import RichText.Model.Element as Element exposing (Element, element)
import RichText.Model.Node as Node
    exposing
        ( Block
        , BlockChildren
        , Children(..)
        , Inline(..)
        , Path
        , block
        , blockChildren
        , childNodes
        , commonAncestor
        , decrement
        , increment
        , toBlockArray
        )
import RichText.Model.Selection
    exposing
        ( Selection
        , anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , isCollapsed
        , normalize
        )
import RichText.Model.State as State exposing (root, withRoot, withSelection)
import RichText.Model.Text exposing (text)
import RichText.Node
    exposing
        ( Fragment(..)
        , Node(..)
        , findAncestor
        , isEmptyTextBlock
        , joinBlocks
        , last
        , nodeAt
        , replace
        , replaceWithFragment
        )


{-| Represents if a list is ordered or unordered.
-}
type ListType
    = Ordered
    | Unordered


{-| A list definition defines which elements represents ordered, unordered, and list items.
-}
type ListDefinition
    = ListDefinition ListDefinitionContents


{-| Creates a list definition
-}
listDefinition : { ordered : Element, unordered : Element, item : Element } -> ListDefinition
listDefinition contents =
    ListDefinition contents


type alias ListDefinitionContents =
    { ordered : Element, unordered : Element, item : Element }


{-| Creates a predefined command map related to lists. You can combine this command map with others
using `RichText.Model.Command.combine`:

    listCommandMap : CommandMap
    listCommandMap = RichText.List.defaultCommandMap defaultListDefinition

    combine listCommandMap RichText.Commands.defaultCommandMap

-}
defaultCommandMap : ListDefinition -> CommandMap
defaultCommandMap definition =
    let
        backspaceCommand =
            joinBackward definition

        deleteCommand =
            joinForward definition
    in
    emptyCommandMap
        |> set [ inputEvent "insertParagraph", key [ enter ], key [ return ] ]
            [ ( "liftEmptyListItem", transform <| liftEmpty definition )
            , ( "splitListItem", transform <| split definition )
            ]
        |> set [ inputEvent "deleteContentBackward", key [ backspace ] ]
            [ ( "joinListBackward", transform <| backspaceCommand ) ]
        |> set [ inputEvent "deleteContentForward", key [ delete ] ]
            [ ( "joinListForward", transform <| deleteCommand ) ]
        |> set [ inputEvent "deleteWordBackward", key [ alt, backspace ] ]
            [ ( "joinListBackward", transform <| backspaceCommand ) ]
        |> set [ inputEvent "deleteWordForward", key [ alt, delete ] ]
            [ ( "joinListForward", transform <| deleteCommand ) ]


{-| The default list definition, which uses `RichText.Definitions` `orderedList`, `unorderedList` and `listItem`
element definitions.
-}
defaultListDefinition : ListDefinition
defaultListDefinition =
    listDefinition
        { ordered = element orderedList []
        , unordered = element unorderedList []
        , item = element listItem []
        }


{-| Retrieves the element template for list items
-}
item : ListDefinition -> Element
item definition =
    case definition of
        ListDefinition c ->
            c.item


{-| Retrieves the element template for ordered lists
-}
ordered : ListDefinition -> Element
ordered definition =
    case definition of
        ListDefinition c ->
            c.ordered


{-| Retrieves the element template for unordered lists
-}
unordered : ListDefinition -> Element
unordered definition =
    case definition of
        ListDefinition c ->
            c.unordered


addListItem : ListDefinition -> Block -> Block
addListItem definition node =
    block
        (item definition)
        (blockChildren <|
            Array.fromList [ node ]
        )


{-| Wraps the selection in a list.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        , block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text2"
                                    ]
                            )
                        ]
                )
            )
            (Just <| range [ 0, 0 ] 0 [ 1, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| range [ 0, 0, 0, 0 ] 0 [ 0, 1, 0, 0 ] 0)

    wrap defaultListDefinition Ordered before == Ok after
    --> True

-}
wrap : ListDefinition -> ListType -> Transform
wrap definition type_ editorState =
    RichText.Commands.wrap (addListItem definition)
        (if type_ == Ordered then
            ordered definition

         else
            unordered definition
        )
        editorState


{-| Finds the closest list item ancestor and path if one exists, otherwise returns `Nothing`.
-}
findListItemAncestor : Element -> Path -> Block -> Maybe ( Path, Block )
findListItemAncestor parameters =
    findAncestor (\n -> Element.name (Node.element n) == Element.name parameters)


{-| Same as `RichText.Commands.splitTextBlock`, but searches for a list item ancestor instead of a text block

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0, 0 ] 2)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "te"
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "xt"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1, 0, 0 ] 0)

    split defaultListDefinition before == Ok after
    --> True

-}
split : ListDefinition -> Transform
split definition =
    RichText.Commands.splitBlock (findListItemAncestor (item definition))


{-| Returns true if the current node is a ordered or unordered list, false otherwise.
-}
isListNode : ListDefinition -> Node -> Bool
isListNode definition node =
    case node of
        Inline _ ->
            False

        Block bn ->
            let
                bnName =
                    Element.name (Node.element bn)
            in
            bnName
                == Element.name (ordered definition)
                || bnName
                == Element.name (unordered definition)


addLiftAnnotationAtPathAndChildren : Path -> Block -> Result String Block
addLiftAnnotationAtPathAndChildren path root =
    case Annotation.addAtPath Annotation.lift path root of
        Err s ->
            Err s

        Ok newRoot ->
            case nodeAt path newRoot of
                Nothing ->
                    Err "Invalid path"

                Just node ->
                    case node of
                        Block bn ->
                            case childNodes bn of
                                BlockChildren ba ->
                                    List.foldl
                                        (\i result ->
                                            case result of
                                                Err _ ->
                                                    result

                                                Ok n ->
                                                    Annotation.addAtPath Annotation.lift (path ++ [ i ]) n
                                        )
                                        (Ok newRoot)
                                        (List.range 0 (Array.length (toBlockArray ba) - 1))

                                _ ->
                                    Err "I was expecting a block array to add a lift mark to"

                        _ ->
                            Err "I was expecting a block node to add a lift mark to"


addLiftMarkToListItems : ListDefinition -> Selection -> Block -> Result String Block
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


{-| Lifts a list item's contents out of its parent list.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0, 0 ] 4)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText "text"
                                    ]
                            )
                        , block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 4)

    lift defaultListDefinition before == Ok after
    --> True

-}
lift : ListDefinition -> Transform
lift definition editorState =
    case State.selection editorState of
        Nothing ->
            Err "Nothing is selected"

        Just selection ->
            let
                normalizedSelection =
                    normalize selection
            in
            case addLiftMarkToListItems definition normalizedSelection <| annotateSelection normalizedSelection (State.root editorState) of
                Err s ->
                    Err s

                Ok markedRoot ->
                    let
                        -- this logic looks suspicious... but it seems to work
                        liftedRoot =
                            doLift <| doLift markedRoot

                        newSelection =
                            selectionFromAnnotations liftedRoot (anchorOffset normalizedSelection) (focusOffset normalizedSelection)
                    in
                    Ok
                        (editorState
                            |> withSelection newSelection
                            |> withRoot (clear Annotation.lift <| clearSelectionAnnotations liftedRoot)
                        )


{-| Same as `lift` but only will succeed if the list item is empty.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText ""
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <|
                                Array.fromList
                                    [ plainText ""
                                    ]
                            )
                        , block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 0)

    liftEmpty defaultListDefinition before == Ok after
    --> True

-}
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
                                case Array.get 0 (toBlockArray a) of
                                    Nothing ->
                                        Err "Cannot lift a list item with no children"

                                    Just firstNode ->
                                        if not <| isEmptyTextBlock (Block firstNode) then
                                            Err "I cannot lift a node that is not an empty text block"

                                        else
                                            lift definition editorState

                            _ ->
                                Err "I was expecting a list item to have block child nodes"


{-| True if the selection is collapsed at the beginning of a list item, false otherwise.
-}
isBeginningOfListItem : ListDefinition -> Selection -> Block -> Bool
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


{-| Joins a list item backwards if the selection is at the beginning of a list item, otherwise
fails with an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 1, 0, 0 ] 0)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                , block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 1, 0 ] 0)

    joinBackward defaultListDefinition before == Ok after
    --> True

-}
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
                        normalize selection

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
                                        Inline _ ->
                                            Err "There is no list item at path"

                                        Block prevBn ->
                                            case joinBlocks prevBn liNode of
                                                Nothing ->
                                                    Err "Could not join list items"

                                                Just joinedLi ->
                                                    let
                                                        joinedNodes =
                                                            replace prevLiPath (Block joinedLi) markedRoot
                                                                |> Result.andThen
                                                                    (replaceWithFragment liPath (BlockFragment Array.empty))
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


{-| True if the selection is collapsed at the end of a list item, false otherwise.
-}
isEndOfListItem : ListDefinition -> Selection -> Block -> Bool
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
                        last node
                in
                if anchorNode selection /= path ++ lastPath then
                    False

                else
                    case lastNode of
                        Inline il ->
                            case il of
                                Text tl ->
                                    String.length (text tl) == anchorOffset selection

                                _ ->
                                    True

                        _ ->
                            True


{-| Joins a list item if the selection is at the end of a list item, otherwise fails with an error.

    before : State
    before =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                ]
                                        )
                                    , block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0, 0 ] 4)


    after : State
    after =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block (Element.element orderedList [])
                            (blockChildren <|
                                Array.fromList <|
                                    [ block
                                        (Element.element listItem [])
                                        (blockChildren <|
                                            Array.fromList
                                                [ block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text"
                                                            ]
                                                    )
                                                , block
                                                    (Element.element paragraph [])
                                                    (inlineChildren <|
                                                        Array.fromList
                                                            [ plainText "text2"
                                                            ]
                                                    )
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            )
            (Just <| caret [ 0, 0, 0, 0 ] 4)

    joinForward defaultListDefinition before == Ok after
    --> True

-}
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
                        normalize selection

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
                                    Inline _ ->
                                        Err "There is no list item at path"

                                    Block nextBn ->
                                        case joinBlocks liNode nextBn of
                                            Nothing ->
                                                Err "I could not join these list items"

                                            Just joinedLi ->
                                                let
                                                    joinedNodes =
                                                        replace liPath (Block joinedLi) markedRoot
                                                            |> Result.andThen
                                                                (replaceWithFragment nextLiPath (BlockFragment Array.empty))
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
