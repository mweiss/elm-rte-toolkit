module RichText.Model.Node exposing
    ( Block, block, element, childNodes, withElement, withChildNodes
    , Children(..), BlockChildren, blockChildren, toBlockArray, InlineChildren, inlineChildren, toInlineArray, toInlineTree, reverseLookup, marksToMarkNodeList
    , Inline(..), InlineTree(..), inlineElement, marks, plainText, markedText
    , Path, parent, increment, decrement, toString, commonAncestor
    )

{-| This module contains types related to the nodes in an editor.

An editor consists of two types of nodes, block and inline. Block nodes are used to represent
hierarchical structures like blockquotes, tables and nested lists. Inline nodes are used to
represent flat structures, like text, inline images, and hard breaks.


# Block

@docs Block, block, element, childNodes, withElement, withChildNodes


# Children

@docs Children, BlockChildren, blockChildren, toBlockArray, InlineChildren, inlineChildren, toInlineArray, toInlineTree, reverseLookup, marksToMarkNodeList


# Inline

@docs Inline, InlineTree, inlineElement, marks, plainText, markedText


# Path

@docs Path, parent, increment, decrement, toString, commonAncestor

-}

import Array exposing (Array)
import List.Extra
import RichText.Model.Element exposing (Element)
import RichText.Model.InlineElement as InlineElement exposing (InlineElement)
import RichText.Model.Mark exposing (Mark, name)
import RichText.Model.Text as Text exposing (Text)


{-| A node path is a list of indexes that represent the path from a node to a child. It's
the main type used to identify where a node is in the editor.
-}
type alias Path =
    List Int


{-| Returns the parent path of the given path

    parent [0, 1, 2]
    --> [0, 1]

-}
parent : Path -> Path
parent path =
    List.take (List.length path - 1) path


{-| Increments the last index in a node path if one exists.

    increment [0, 1]
    --> [0, 2]

-}
increment : Path -> Path
increment np =
    case List.Extra.last np of
        Nothing ->
            []

        Just i ->
            List.take (List.length np - 1) np ++ [ i + 1 ]


{-| Decrements the last index in a node path if one exists.

    decrement [0, 0]
    --> [0, 0]

-}
decrement : Path -> Path
decrement np =
    case List.Extra.last np of
        Nothing ->
            []

        Just i ->
            List.take (List.length np - 1) np ++ [ i - 1 ]


{-| String representation of a path.

    toString [0, 0]
    --> "0:0"

-}
toString : Path -> String
toString nodePath =
    String.join ":" <| List.map String.fromInt nodePath


{-| Returns the common ancestor of the two paths.

    commonAncestor [0, 1, 2] [0, 1, 4]
    --> [0, 1]

-}
commonAncestor : Path -> Path -> Path
commonAncestor xPath yPath =
    case xPath of
        [] ->
            []

        x :: xs ->
            case yPath of
                [] ->
                    []

                y :: ys ->
                    if x == y then
                        x :: commonAncestor xs ys

                    else
                        []


{-| A `Block` represents a block element in your document. An block can either
have other block nodes as children, have all inline leaf nodes as children (e.g a text block),
or be a leaf node.
-}
type Block
    = Block BlockNodeContents


type alias BlockNodeContents =
    { parameters : Element
    , childNodes : Children
    }


{-| Creates a block node. The arguments are as follows

  - `element` is the element this block node represents
  - `childNodes` are the children related to this block node.

```
block
    (Element.element paragraph [])
    (inlineChildren <| Array.fromList [ plainText "some text" ])
```

-}
block : Element -> Children -> Block
block parameters cn =
    Block { parameters = parameters, childNodes = cn }


{-| the element from a block node
-}
element : Block -> Element
element node =
    case node of
        Block n ->
            n.parameters


{-| the childNodes from a block node.
-}
childNodes : Block -> Children
childNodes node =
    case node of
        Block n ->
            n.childNodes


{-| a block node with the given element set
-}
withElement : Element -> Block -> Block
withElement parameters node =
    case node of
        Block c ->
            Block { c | parameters = parameters }


{-| a block node with the given children set
-}
withChildNodes : Children -> Block -> Block
withChildNodes cn node =
    case node of
        Block n ->
            Block { n | childNodes = cn }


{-| Children represents what children an editor block node can have. A block node may have
other block nodes as children, inline leaf nodes as children, or it may be a leaf itself.
-}
type Children
    = BlockChildren BlockChildren
    | InlineChildren InlineChildren
    | Leaf


{-| `BlockChildren` are child nodes that are all blocks.
-}
type BlockChildren
    = BlockArray (Array Block)


{-| Creates children from a block array

    blockChildren (Array.fromList [ paragraphNode ])

-}
blockChildren : Array Block -> Children
blockChildren arr =
    BlockChildren <| BlockArray arr


{-| Returns a block array from block children
-}
toBlockArray : BlockChildren -> Array Block
toBlockArray arr =
    case arr of
        BlockArray a ->
            a


{-| `InlineChildren` are child nodes that are all inline. Internally, it's represented as both
a flat structure (which can be accessed via `inlineArray`), and a hierarchical structure
(which can be accessed via `inlineTree`).
-}
type InlineChildren
    = InlineLeafArray InlineLeafArrayContents


{-| an array of inline nodes (flat structure)
-}
toInlineArray : InlineChildren -> Array Inline
toInlineArray arr =
    case arr of
        InlineLeafArray a ->
            a.array


{-| a tree of mark nodes with inline leaf indices
-}
toInlineTree : InlineChildren -> Array InlineTree
toInlineTree arr =
    case arr of
        InlineLeafArray a ->
            a.tree


{-| a lookup array that maps the index of the inline array to its path in the inline tree
-}
reverseLookup : InlineChildren -> Array Path
reverseLookup arr =
    case arr of
        InlineLeafArray a ->
            a.reverseLookup


type alias InlineLeafArrayContents =
    { array : Array Inline, tree : Array InlineTree, reverseLookup : Array Path }


{-| An inline tree is the nested structure of an inline array. Because mark when rendered can span
multiple inline nodes, inline content is still technically hierarchical. When rendering or
parsing, it can be useful to see this information as a tree instead of an array.

  - `MarkNode` node that represents a mark and its inline children
  - `LeafNode` the index of the inline in the inline array

-}
type InlineTree
    = MarkNode { mark : Mark, children : Array InlineTree }
    | LeafNode Int


{-| An inline leaf node represents an inline element in your document. It can either be an inline
leaf node, like an image or line break, or a text node.
-}
type Inline
    = InlineElement InlineElement
    | Text Text


{-| A Inline that represents plain text
-}
plainText : String -> Inline
plainText s =
    Text (Text.empty |> Text.withText s)


{-| Creates children derived from an inline array.
-}
inlineChildren : Array Inline -> Children
inlineChildren arr =
    let
        tree =
            marksToMarkNodeList (List.map marks (Array.toList arr))
    in
    InlineChildren <|
        InlineLeafArray
            { array = arr
            , tree = tree
            , reverseLookup = Array.fromList <| inlineTreeToPaths [] tree
            }


inlineTreeToPaths : Path -> Array InlineTree -> List Path
inlineTreeToPaths backwardsPath tree =
    List.concatMap
        (\( i, n ) ->
            case n of
                LeafNode _ ->
                    [ List.reverse (i :: backwardsPath) ]

                MarkNode mn ->
                    inlineTreeToPaths (i :: backwardsPath) mn.children
        )
        (List.indexedMap Tuple.pair (Array.toList tree))


{-| Derives the marks from an inline node
-}
marks : Inline -> List Mark
marks leaf =
    case leaf of
        Text l ->
            Text.marks l

        InlineElement l ->
            InlineElement.marks l


{-| Transforms a list of list of marks to an array of inline tree nodes
-}
marksToMarkNodeList : List (List Mark) -> Array InlineTree
marksToMarkNodeList markLists =
    marksToMarkNodeListRec (List.indexedMap Tuple.pair markLists)


marksToMarkNodeListRec : List ( Int, List Mark ) -> Array InlineTree
marksToMarkNodeListRec indexedMarkLists =
    Array.fromList <|
        List.concatMap
            (\( ( i, ( m, rest ) ), groupRest ) ->
                case m of
                    Nothing ->
                        LeafNode i :: List.map (\( j, _ ) -> LeafNode j) groupRest

                    Just mk ->
                        [ MarkNode
                            { mark = mk
                            , children = marksToMarkNodeListRec (( i, rest ) :: List.map (\( j, ( _, r ) ) -> ( j, r )) groupRest)
                            }
                        ]
            )
        <|
            List.Extra.groupWhile
                (\( _, ( m1, _ ) ) ( _, ( m2, _ ) ) ->
                    case m1 of
                        Nothing ->
                            case m2 of
                                Nothing ->
                                    True

                                _ ->
                                    False

                        Just v1 ->
                            case m2 of
                                Just v2 ->
                                    name v1 == name v2

                                _ ->
                                    False
                )
            <|
                List.map (\( i, a ) -> ( i, ( List.head a, List.drop 1 a ) )) indexedMarkLists


{-| Creates an `Inline` from an `Element` and `Mark`
-}
inlineElement : Element -> List Mark -> Inline
inlineElement parameters mark =
    InlineElement (InlineElement.inlineElement parameters mark)


{-| Creates an inline that represents some text with marks
-}
markedText : String -> List Mark -> Inline
markedText s marks_ =
    Text
        (Text.empty
            |> Text.withText s
            |> Text.withMarks marks_
        )
