module RichText.State exposing
    ( reduce, validate
    , translateReducedTextBlockSelection
    )

{-| This module contains functions to validate and reduce editor state. These methods are used
every time a command is applied.


# State functions

@docs reduce, validate


# Helpers

@docs translateReducedTextBlockSelection

-}

import Array exposing (Array)
import List.Extra
import RichText.Annotation exposing (annotateSelection, clearSelectionAnnotations, selection)
import RichText.Config.ElementDefinition as ElementDefinition
import RichText.Config.Spec exposing (Spec)
import RichText.Internal.Definitions exposing (ContentType(..), toStringContentType)
import RichText.Internal.Spec exposing (elementDefinitionWithDefault)
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark as Mark
import RichText.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , InlineChildren
        , Path
        , childNodes
        , element
        , inlineChildren
        , toBlockArray
        , toInlineArray
        , withChildNodes
        )
import RichText.Model.Selection
    exposing
        ( anchorNode
        , anchorOffset
        , focusNode
        , focusOffset
        , range
        )
import RichText.Model.State as State exposing (State, withRoot, withSelection)
import RichText.Model.Text as Text exposing (marks, text, withText)
import RichText.Node exposing (Node(..), findTextBlockNodeAncestor, map)
import Set exposing (Set)


removeExtraEmptyTextLeaves : List Inline -> List Inline
removeExtraEmptyTextLeaves inlineLeaves =
    case inlineLeaves of
        [] ->
            inlineLeaves

        [ _ ] ->
            inlineLeaves

        x :: y :: xs ->
            case x of
                Text xL ->
                    case y of
                        Text yL ->
                            if String.isEmpty (text xL) && (not <| Set.member selection (Text.annotations xL)) then
                                removeExtraEmptyTextLeaves (y :: xs)

                            else if String.isEmpty (text yL) && (not <| Set.member selection (Text.annotations yL)) then
                                removeExtraEmptyTextLeaves (x :: xs)

                            else
                                x :: removeExtraEmptyTextLeaves (y :: xs)

                        InlineElement _ ->
                            x :: removeExtraEmptyTextLeaves (y :: xs)

                InlineElement _ ->
                    x :: removeExtraEmptyTextLeaves (y :: xs)


mergeSimilarInlineLeaves : List Inline -> List Inline
mergeSimilarInlineLeaves inlineLeaves =
    case inlineLeaves of
        [] ->
            inlineLeaves

        [ _ ] ->
            inlineLeaves

        x :: y :: xs ->
            case x of
                Text xL ->
                    case y of
                        Text yL ->
                            if marks xL == marks yL then
                                mergeSimilarInlineLeaves (Text (xL |> withText (text xL ++ text yL)) :: xs)

                            else
                                x :: mergeSimilarInlineLeaves (y :: xs)

                        InlineElement _ ->
                            x :: mergeSimilarInlineLeaves (y :: xs)

                InlineElement _ ->
                    x :: mergeSimilarInlineLeaves (y :: xs)


reduceNode : Block -> Block
reduceNode node =
    case
        map
            (\x ->
                case x of
                    Block bn ->
                        case childNodes bn of
                            InlineChildren a ->
                                Block <|
                                    (bn
                                        |> withChildNodes
                                            (inlineChildren <|
                                                Array.fromList
                                                    (mergeSimilarInlineLeaves (removeExtraEmptyTextLeaves (Array.toList (toInlineArray a))))
                                            )
                                    )

                            _ ->
                                x

                    _ ->
                        x
            )
            (Block node)
    of
        Block newNode ->
            newNode

        _ ->
            node


{-| Reduces the state with the following rules:

  - Neighboring text nodes with the same marks are merged into one text node
  - Empty text nodes (regardless of marks) that are not part of the current collapsed
    selection are removed if there is another neighboring text node

```
before : State
before =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "te", plainText "xt" ])
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 2)


after : State
after =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)

reduce before == after
--> True
```

-}
reduce : State -> State
reduce editorState =
    let
        markedRoot =
            case State.selection editorState of
                Nothing ->
                    State.root editorState

                Just selection ->
                    annotateSelection selection (State.root editorState)

        reducedRoot =
            clearSelectionAnnotations <| reduceNode markedRoot
    in
    translateReducedTextBlockSelection reducedRoot editorState


{-| Just the selection translation function that gets called in `reduce`. Note that this is really only
useful if you're creating transforms that merge or remove inline nodes and you can't find a way
to easily figure out the new selection state.
-}
translateReducedTextBlockSelection : Block -> State -> State
translateReducedTextBlockSelection root state =
    case State.selection state of
        Nothing ->
            state |> withRoot root

        Just selection ->
            let
                ( aP, aO ) =
                    translatePath (State.root state) root (anchorNode selection) (anchorOffset selection)

                ( fP, fO ) =
                    translatePath (State.root state) root (focusNode selection) (focusOffset selection)
            in
            state
                |> withRoot root
                |> withSelection (Just <| range aP aO fP fO)


translatePath : Block -> Block -> Path -> Int -> ( Path, Int )
translatePath old new path offset =
    case findTextBlockNodeAncestor path old of
        Nothing ->
            ( path, offset )

        Just ( _, oldN ) ->
            case findTextBlockNodeAncestor path new of
                Nothing ->
                    ( path, offset )

                Just ( _, newN ) ->
                    if oldN == newN then
                        ( path, offset )

                    else
                        case childNodes oldN of
                            InlineChildren oldA ->
                                case List.Extra.last path of
                                    Nothing ->
                                        ( path, offset )

                                    Just lastIndex ->
                                        case childNodes newN of
                                            InlineChildren newA ->
                                                let
                                                    pOff =
                                                        parentOffset (toInlineArray oldA) lastIndex offset

                                                    ( cI, cO ) =
                                                        childOffset (toInlineArray newA) pOff

                                                    newPath =
                                                        List.take (List.length path - 1) path ++ [ cI ]
                                                in
                                                ( newPath, cO )

                                            _ ->
                                                ( path, offset )

                            _ ->
                                ( path, offset )


parentOffset : Array Inline -> Int -> Int -> Int
parentOffset leaves index offset =
    let
        ( _, newOffset ) =
            Array.foldl
                (\l ( i, accOffset ) ->
                    case l of
                        Text tl ->
                            ( i + 1
                            , if i < index then
                                accOffset + String.length (text tl)

                              else
                                accOffset
                            )

                        InlineElement _ ->
                            ( i + 1
                            , if i < index then
                                accOffset + 1

                              else
                                accOffset
                            )
                )
                ( 0, offset )
                leaves
    in
    newOffset


childOffset : Array Inline -> Int -> ( Int, Int )
childOffset leaves offset =
    let
        ( newIndex, newOffset, _ ) =
            Array.foldl
                (\l ( i, accOffset, done ) ->
                    if done then
                        ( i, accOffset, done )

                    else if accOffset <= 0 then
                        ( i, accOffset, True )

                    else
                        case l of
                            Text tl ->
                                if accOffset <= String.length (text tl) then
                                    ( i, accOffset, True )

                                else
                                    ( i + 1, accOffset - String.length (text tl), False )

                            InlineElement _ ->
                                ( i + 1, accOffset - 1, False )
                )
                ( 0, offset, False )
                leaves
    in
    ( newIndex, newOffset )


{-| Validates the state against the spec and returns the valid state if everything is okay, otherwise
returns a comma separated string of error messages.

    example : State
    example =
        state
            (block
                (Element.element doc [])
                (blockChildren <|
                    Array.fromList
                        [ block
                            (Element.element paragraph [])
                            (inlineChildren <| Array.fromList [ plainText "text" ])
                        ]
                )
            )
            (Just <| caret [ 0, 0 ] 2)

    (Ok example) == (validate markdown example)
    --> True

-}
validate : Spec -> State -> Result String State
validate spec editorState =
    let
        root =
            State.root editorState
    in
    case validateEditorBlockNode spec (Just <| Set.singleton "root") root of
        [] ->
            Ok editorState

        result ->
            Err <| String.join ", " result


validateAllowedMarks : Maybe (Set String) -> Inline -> List String
validateAllowedMarks allowedMarks leaf =
    case allowedMarks of
        Nothing ->
            []

        Just allowed ->
            let
                notAllowed =
                    Set.diff (Set.fromList (List.map (\m -> Mark.name m) (Node.marks leaf))) allowed
            in
            if Set.isEmpty notAllowed then
                []

            else
                [ "Inline node is only allowed the following marks: "
                    ++ String.join "," (Set.toList allowed)
                    ++ ", but found "
                    ++ String.join "," (Set.toList notAllowed)
                ]


validateInlineLeaf : Spec -> Maybe (Set String) -> Maybe (Set String) -> Inline -> List String
validateInlineLeaf spec allowedGroups allowedMarks leaf =
    validateAllowedMarks allowedMarks leaf
        ++ (case leaf of
                Node.Text _ ->
                    []

                Node.InlineElement il ->
                    let
                        definition =
                            elementDefinitionWithDefault (InlineElement.element il) spec
                    in
                    validateAllowedGroups allowedGroups (ElementDefinition.group definition) (ElementDefinition.name definition)
           )


validateAllowedGroups : Maybe (Set String) -> String -> String -> List String
validateAllowedGroups allowedGroups group name =
    case allowedGroups of
        Nothing ->
            []

        Just groups ->
            if Set.member group groups then
                []

            else if Set.member name groups then
                []

            else
                [ "Group "
                    ++ group
                    ++ " is not in allowed groups ["
                    ++ String.join ", " (Set.toList groups)
                    ++ "]"
                ]


validateEditorBlockNode : Spec -> Maybe (Set String) -> Block -> List String
validateEditorBlockNode spec allowedGroups node =
    let
        parameters =
            element node

        definition =
            elementDefinitionWithDefault parameters spec
    in
    let
        allowedGroupsErrors =
            validateAllowedGroups allowedGroups (ElementDefinition.group definition) (ElementDefinition.name definition)
    in
    if not <| List.isEmpty allowedGroupsErrors then
        allowedGroupsErrors

    else
        let
            contentType =
                ElementDefinition.contentType definition
        in
        case childNodes node of
            BlockChildren ba ->
                case contentType of
                    BlockNodeType groups ->
                        List.concatMap
                            (validateEditorBlockNode spec groups)
                            (Array.toList (toBlockArray ba))

                    _ ->
                        [ "I was expecting textblock content type, but instead I got "
                            ++ toStringContentType contentType
                        ]

            InlineChildren la ->
                case contentType of
                    TextBlockNodeType config ->
                        List.concatMap (validateInlineLeaf spec config.allowedGroups config.allowedMarks) (Array.toList (toInlineArray la))

                    _ ->
                        [ "I was expecting textblock content type, but instead I got " ++ toStringContentType contentType ]

            Leaf ->
                if contentType == ElementDefinition.blockLeaf then
                    []

                else
                    [ "I was expecting leaf blockleaf content type, but instead I got "
                        ++ toStringContentType contentType
                    ]
