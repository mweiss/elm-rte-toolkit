module RichTextEditor.Internal.Model.Node exposing
    ( BlockArray
    , ChildNodes(..)
    , EditorBlockNode
    , EditorFragment(..)
    , EditorNode(..)
    , ElementParameters
    , InlineLeafArray
    , InlineLeafParameters
    , NodePath
    , TextLeafParameters
    , editorBlockNode
    , elementParameters
    , inlineLeafArray
    )

{-| A node path is a list of indexes that represent the path from an editor fragment to a node. It's
the main type used to identify where a node is in the editor.
-}

import Array exposing (Array)
import List.Extra
import RichTextEditor.Internal.Model.Annotation exposing (Annotation)
import RichTextEditor.Internal.Model.Attribute exposing (Attribute)
import RichTextEditor.Internal.Model.Mark exposing (Mark)
import Set exposing (Set)


type alias NodePath =
    List Int


type EditorNode
    = BlockNodeWrapper EditorBlockNode
    | InlineLeafWrapper EditorInlineLeaf


type EditorFragment
    = BlockNodeFragment (Array EditorBlockNode)
    | InlineLeafFragment (Array EditorInlineLeaf)


type ElementParameters
    = ElementParameters ElementParametersContents


type alias ElementParametersContents =
    { name : String
    , attributes : List Attribute
    , annotations : Set Annotation
    }


elementParameters : String -> List Attribute -> Set Annotation -> ElementParameters
elementParameters name attributes annotations =
    ElementParameters { name = name, attributes = attributes, annotations = annotations }


{-| An editor block node represents a block element in your document. An editor block node can either
have other block nodes as children, have all inline leaf nodes as children, or be a leaf node.
-}
type EditorBlockNode
    = EditorBlockNode EditorBlockNodeContents


type alias EditorBlockNodeContents =
    { parameters : ElementParameters
    , childNodes : ChildNodes
    }


editorBlockNode : ElementParameters -> ChildNodes -> EditorBlockNode
editorBlockNode parameters childNodes =
    EditorBlockNode { parameters = parameters, childNodes = childNodes }


{-| ChildNodes represents what children an editor block node can have. A block node may have
other block nodes as children, inline leaf nodes as children, or it may be a leaf itself.
-}
type ChildNodes
    = BlockChildren BlockArray
    | InlineChildren InlineLeafArray
    | Leaf


type BlockArray
    = BlockArray (Array EditorBlockNode)


arrayFromBlockArray : BlockArray -> Array EditorBlockNode
arrayFromBlockArray blockArray =
    case blockArray of
        BlockArray a ->
            a


type alias MarkNodeContents =
    { mark : Mark, children : Array InlineLeafTree }


type InlineLeafArray
    = InlineLeafArray InlineLeafArrayContents


type alias InlineLeafArrayContents =
    { array : Array EditorInlineLeaf, tree : Array InlineLeafTree, reverseLookup : Array NodePath }


type InlineLeafTree
    = MarkNode MarkNodeContents
    | LeafNode Int


type InlineLeafParameters
    = InlineLeafParameters InlineLeafParametersContents


type alias InlineLeafParametersContents =
    { marks : List Mark
    , parameters : ElementParameters
    }


marksFromInlineLeafParameters : InlineLeafParameters -> List Mark
marksFromInlineLeafParameters parameters =
    case parameters of
        InlineLeafParameters c ->
            c.marks


elementParametersFromInlineLeafParameters : InlineLeafParameters -> ElementParameters
elementParametersFromInlineLeafParameters parameters =
    case parameters of
        InlineLeafParameters c ->
            c.parameters


{-| An inline leaf node represents an inline element in your document. It can either be an inline
leaf node, like an image or line break, or a text node.
-}
type EditorInlineLeaf
    = InlineLeaf InlineLeafParameters
    | TextLeaf TextLeafParameters


{-| TextNodeContents represents the attributes that can be in a text node. The core attributes
are marks and text.
-}
type TextLeafParameters
    = TextLeafParameters TextLeafParametersContents


marksFromTextLeafParameters : TextLeafParameters -> List Mark
marksFromTextLeafParameters parameters =
    case parameters of
        TextLeafParameters c ->
            c.marks


text : TextLeafParameters -> String
text parameters =
    case parameters of
        TextLeafParameters c ->
            c.text


type alias TextLeafParametersContents =
    { marks : List Mark
    , annotations : Set Annotation
    , text : String
    }


inlineLeafArray : Array EditorInlineLeaf -> ChildNodes
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


inlineLeafTreeToPaths : NodePath -> Array InlineLeafTree -> List NodePath
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


marksFromInlineLeaf : EditorInlineLeaf -> List Mark
marksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            marksFromTextLeafParameters l

        InlineLeaf l ->
            marksFromInlineLeafParameters l


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
                        LeafNode i :: List.map (\( j, _ ) -> LeafNode <| j) groupRest

                    Just mk ->
                        [ MarkNode
                            { mark = mk
                            , children = marksToMarkNodeListRec (( i, rest ) :: List.map (\( j, ( _, r ) ) -> ( j, r )) groupRest)
                            }
                        ]
            )
        <|
            List.Extra.groupWhile
                (\( _, ( m1, _ ) ) ( _, ( m2, _ ) ) -> m1 == m2)
            <|
                List.map (\( i, a ) -> ( i, ( List.head a, List.drop 1 a ) )) indexedMarkLists
