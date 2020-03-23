module RichTextEditor.Model.Node exposing
    ( Block
    , BlockChildren
    , Children(..)
    , Inline(..)
    , InlineChildren
    , InlineTree(..)
    , MarkNodeContents
    , Path
    , blockNode
    , childNodes
    , elementFromBlockNode
    , fromBlockArray
    , inlineArray
    , inlineChildren
    , inlineElement
    , inlineTree
    , markedText
    , marksFromInline
    , marksToMarkNodeList
    , parent
    , plainText
    , reverseLookup
    , toBlockArray
    , withChildNodes
    , withElement
    )

{-| A node path is a list of indexes that represent the path from an editor fragment to a node. It's
the main type used to identify where a node is in the editor.
-}

import Array exposing (Array)
import Array.Extra as Array
import List.Extra
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.InlineElement as InlineElement exposing (InlineElement)
import RichTextEditor.Model.Mark exposing (Mark, name)
import RichTextEditor.Model.Text as Text exposing (Text)


type alias Path =
    List Int


parent : Path -> Path
parent path =
    List.take (List.length path - 1) path


{-| An editor block node represents a block element in your document. An editor block node can either
have other block nodes as children, have all inline leaf nodes as children, or be a leaf node.
-}
type Block
    = Block BlockNodeContents


type alias BlockNodeContents =
    { parameters : Element
    , childNodes : Children
    }


blockNode : Element -> Children -> Block
blockNode parameters cn =
    Block { parameters = parameters, childNodes = cn }


elementFromBlockNode : Block -> Element
elementFromBlockNode node =
    case node of
        Block n ->
            n.parameters


childNodes : Block -> Children
childNodes node =
    case node of
        Block n ->
            n.childNodes


withElement : Element -> Block -> Block
withElement parameters node =
    case node of
        Block c ->
            Block { c | parameters = parameters }


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


type BlockChildren
    = BlockArray (Array Block)


fromBlockArray : Array Block -> Children
fromBlockArray arr =
    BlockChildren <| BlockArray arr


toBlockArray : BlockChildren -> Array Block
toBlockArray arr =
    case arr of
        BlockArray a ->
            a


type alias MarkNodeContents =
    { mark : Mark, children : Array InlineTree }


type InlineChildren
    = InlineLeafArray InlineLeafArrayContents


inlineArray : InlineChildren -> Array Inline
inlineArray arr =
    case arr of
        InlineLeafArray a ->
            a.array


inlineTree : InlineChildren -> Array InlineTree
inlineTree arr =
    case arr of
        InlineLeafArray a ->
            a.tree


reverseLookup : InlineChildren -> Array Path
reverseLookup arr =
    case arr of
        InlineLeafArray a ->
            a.reverseLookup


type alias InlineLeafArrayContents =
    { array : Array Inline, tree : Array InlineTree, reverseLookup : Array Path }


type InlineTree
    = MarkNode MarkNodeContents
    | LeafNode Int


{-| An inline leaf node represents an inline element in your document. It can either be an inline
leaf node, like an image or line break, or a text node.
-}
type Inline
    = InlineElement InlineElement
    | Text Text


plainText : String -> Inline
plainText s =
    Text (Text.empty |> Text.withText s)


inlineChildren : Array Inline -> Children
inlineChildren arr =
    let
        tree =
            marksToMarkNodeList (List.map marksFromInline (Array.toList arr))
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


marksFromInline : Inline -> List Mark
marksFromInline leaf =
    case leaf of
        Text l ->
            Text.marks l

        InlineElement l ->
            InlineElement.marks l


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


inlineElement : Element -> List Mark -> Inline
inlineElement parameters mark =
    InlineElement (InlineElement.inlineElement parameters mark)


markedText : String -> List Mark -> Inline
markedText s marks =
    Text
        (Text.empty
            |> Text.withText s
            |> Text.withMarks marks
        )
