module RichTextEditor.Model.Node exposing
    ( BlockArray
    , BlockNode
    , ChildNodes(..)
    , InlineLeaf(..)
    , InlineLeafArray
    , InlineLeafTree(..)
    , MarkNodeContents
    , Path
    , annotationsFromBlockNode
    , blockArray
    , blockNode
    , blockNodeWithElement
    , childNodes
    , elementFromBlockNode
    , fromBlockArray
    , fromInlineArray
    , inlineLeaf
    , inlineLeafArray
    , isSameBlockNode
    , isSameChildNodes
    , isSameInlineLeaf
    , marksFromInlineLeaf
    , marksToMarkNodeList
    , parent
    , reverseLookupFromInlineArray
    , textLeaf
    , textLeafWithText
    , treeFromInlineArray
    , withChildNodes
    )

{-| A node path is a list of indexes that represent the path from an editor fragment to a node. It's
the main type used to identify where a node is in the editor.
-}

import Array exposing (Array)
import Array.Extra as Array
import List.Extra
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.InlineElement as InlineElement exposing (InlineElement, inlineElement)
import RichTextEditor.Model.Internal.Spec exposing (NodeDefinition, annotationsFromElement)
import RichTextEditor.Model.Mark exposing (Mark, attributes, name)
import RichTextEditor.Model.Text as Text exposing (Text)
import Set exposing (Set)


type alias Path =
    List Int


blockNodeWithElement : Element -> BlockNode -> BlockNode
blockNodeWithElement parameters node =
    case node of
        BlockNode c ->
            BlockNode { c | parameters = parameters }


annotationsFromBlockNode : BlockNode -> Set String
annotationsFromBlockNode node =
    annotationsFromElement <| elementFromBlockNode node


{-| An editor block node represents a block element in your document. An editor block node can either
have other block nodes as children, have all inline leaf nodes as children, or be a leaf node.
-}
type BlockNode
    = BlockNode BlockNodeContents


type alias BlockNodeContents =
    { parameters : Element
    , childNodes : ChildNodes
    }


blockNode : Element -> ChildNodes -> BlockNode
blockNode parameters cn =
    BlockNode { parameters = parameters, childNodes = cn }


withChildNodes : ChildNodes -> BlockNode -> BlockNode
withChildNodes cn node =
    case node of
        BlockNode n ->
            BlockNode { n | childNodes = cn }


elementFromBlockNode : BlockNode -> Element
elementFromBlockNode node =
    case node of
        BlockNode n ->
            n.parameters


childNodes : BlockNode -> ChildNodes
childNodes node =
    case node of
        BlockNode n ->
            n.childNodes


{-| ChildNodes represents what children an editor block node can have. A block node may have
other block nodes as children, inline leaf nodes as children, or it may be a leaf itself.
-}
type ChildNodes
    = BlockChildren BlockArray
    | InlineChildren InlineLeafArray
    | Leaf


type BlockArray
    = BlockArray (Array BlockNode)


blockArray : Array BlockNode -> ChildNodes
blockArray arr =
    BlockChildren <| BlockArray arr


fromBlockArray : BlockArray -> Array BlockNode
fromBlockArray arr =
    case arr of
        BlockArray a ->
            a


type alias MarkNodeContents =
    { mark : Mark, children : Array InlineLeafTree }


type InlineLeafArray
    = InlineLeafArray InlineLeafArrayContents


fromInlineArray : InlineLeafArray -> Array InlineLeaf
fromInlineArray arr =
    case arr of
        InlineLeafArray a ->
            a.array


treeFromInlineArray : InlineLeafArray -> Array InlineLeafTree
treeFromInlineArray arr =
    case arr of
        InlineLeafArray a ->
            a.tree


reverseLookupFromInlineArray : InlineLeafArray -> Array Path
reverseLookupFromInlineArray arr =
    case arr of
        InlineLeafArray a ->
            a.reverseLookup


type alias InlineLeafArrayContents =
    { array : Array InlineLeaf, tree : Array InlineLeafTree, reverseLookup : Array Path }


type InlineLeafTree
    = MarkNode MarkNodeContents
    | LeafNode Int


{-| An inline leaf node represents an inline element in your document. It can either be an inline
leaf node, like an image or line break, or a text node.
-}
type InlineLeaf
    = ElementLeaf InlineElement
    | TextLeaf Text


textLeafWithText : String -> InlineLeaf
textLeafWithText s =
    TextLeaf (Text.empty |> Text.withText s)


inlineLeafArray : Array InlineLeaf -> ChildNodes
inlineLeafArray arr =
    let
        tree =
            marksToMarkNodeList (List.map marksFromInlineLeaf (Array.toList arr))
    in
    InlineChildren <|
        InlineLeafArray
            { array = arr
            , tree = tree
            , reverseLookup = Array.fromList <| inlineLeafTreeToPaths [] tree
            }


inlineLeafTreeToPaths : Path -> Array InlineLeafTree -> List Path
inlineLeafTreeToPaths backwardsPath tree =
    List.concatMap
        (\( i, n ) ->
            case n of
                LeafNode _ ->
                    [ List.reverse (i :: backwardsPath) ]

                MarkNode mn ->
                    inlineLeafTreeToPaths (i :: backwardsPath) mn.children
        )
        (List.indexedMap Tuple.pair (Array.toList tree))


marksFromInlineLeaf : InlineLeaf -> List Mark
marksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            Text.marks l

        ElementLeaf l ->
            InlineElement.marks l


marksToMarkNodeList : List (List Mark) -> Array InlineLeafTree
marksToMarkNodeList markLists =
    marksToMarkNodeListRec (List.indexedMap Tuple.pair markLists)


marksToMarkNodeListRec : List ( Int, List Mark ) -> Array InlineLeafTree
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


isSameInlineLeaf : InlineLeaf -> InlineLeaf -> Bool
isSameInlineLeaf i1 i2 =
    case i1 of
        ElementLeaf il1 ->
            case i2 of
                ElementLeaf il2 ->
                    InlineElement.comparableMarks il1
                        == InlineElement.comparableMarks il2
                        && Element.comparableElement (InlineElement.element il1)
                        == Element.comparableElement (InlineElement.element il2)

                _ ->
                    False

        TextLeaf tl1 ->
            case i2 of
                TextLeaf tl2 ->
                    Text.comparableMarks tl1
                        == Text.comparableMarks tl2
                        && Text.text tl1
                        == Text.text tl2

                _ ->
                    False


isSameChildNodes : ChildNodes -> ChildNodes -> Bool
isSameChildNodes cn1 cn2 =
    case cn1 of
        BlockChildren c1 ->
            case cn2 of
                BlockChildren c2 ->
                    List.all (\( b1, b2 ) -> isSameBlockNode b1 b2)
                        (Array.toList
                            (Array.map2
                                Tuple.pair
                                (fromBlockArray c1)
                                (fromBlockArray c2)
                            )
                        )

                _ ->
                    False

        InlineChildren c1 ->
            case cn2 of
                InlineChildren c2 ->
                    List.all (\( i1, i2 ) -> isSameInlineLeaf i1 i2)
                        (Array.toList
                            (Array.map2
                                Tuple.pair
                                (fromInlineArray c1)
                                (fromInlineArray c2)
                            )
                        )

                _ ->
                    False

        Leaf ->
            case cn2 of
                Leaf ->
                    True

                _ ->
                    False


isSameBlockNode : BlockNode -> BlockNode -> Bool
isSameBlockNode bn1 bn2 =
    let
        e1 =
            Element.comparableElement <| elementFromBlockNode bn1

        e2 =
            Element.comparableElement <| elementFromBlockNode bn2
    in
    if e1 /= e2 then
        False

    else
        isSameChildNodes (childNodes bn1) (childNodes bn2)


inlineLeaf : Element -> List Mark -> InlineLeaf
inlineLeaf parameters mark =
    ElementLeaf (inlineElement parameters mark)


textLeaf : String -> List Mark -> InlineLeaf
textLeaf s marks =
    TextLeaf
        (Text.empty
            |> Text.withText s
            |> Text.withMarks marks
        )


parent : Path -> Path
parent path =
    List.take (List.length path - 1) path
