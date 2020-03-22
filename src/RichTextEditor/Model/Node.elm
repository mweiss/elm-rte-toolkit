module RichTextEditor.Model.Node exposing
    ( BlockArray
    , BlockNode
    , ChildNodes(..)
    , Element
    , InlineLeaf(..)
    , InlineLeafArray
    , InlineLeafParameters
    , InlineLeafTree(..)
    , MarkNodeContents
    , Path
    , TextLeafParameters
    , annotationsFromBlockNode
    , annotationsFromElementParameters
    , annotationsFromTextLeafParameters
    , attributesFromElementParameters
    , blockArray
    , blockNode
    , blockNodeWithElementParameters
    , childNodes
    , comparableElementParameters
    , comparableMarksFromTextLeafParameters
    , definitionFromElementParameters
    , elementParameters
    , elementParametersFromBlockNode
    , elementParametersFromInlineLeafParameters
    , elementParametersWithAnnotations
    , elementParametersWithAttributes
    , elementParametersWithDefinition
    , emptyTextLeafParameters
    , fromBlockArray
    , fromInlineArray
    , inlineLeaf
    , inlineLeafArray
    , inlineLeafParameters
    , inlineLeafParametersWithElementParameters
    , inlineLeafParametersWithMarks
    , isSameBlockNode
    , isSameChildNodes
    , isSameInlineLeaf
    , marksFromInlineLeaf
    , marksFromInlineLeafParameters
    , marksFromTextLeafParameters
    , marksToMarkNodeList
    , nameFromElementParameters
    , parent
    , reverseLookupFromInlineArray
    , text
    , textLeaf
    , textLeafParametersWithAnnotations
    , textLeafParametersWithMarks
    , textLeafWithText
    , treeFromInlineArray
    , withChildNodes
    , withText
    )

{-| A node path is a list of indexes that represent the path from an editor fragment to a node. It's
the main type used to identify where a node is in the editor.
-}

import Array exposing (Array)
import Array.Extra as Array
import List.Extra
import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Internal.Spec as Spec exposing (NodeDefinition)
import RichTextEditor.Model.Mark exposing (Mark, attributes, name)
import RichTextEditor.Model.Spec exposing (nameFromNodeDefinition)
import Set exposing (Set)


type alias Path =
    List Int


type alias Element =
    Spec.Element


annotationsFromElementParameters : Element -> Set String
annotationsFromElementParameters =
    Spec.annotationsFromElementParameters


attributesFromElementParameters : Element -> List Attribute
attributesFromElementParameters =
    Spec.attributesFromElementParameters


definitionFromElementParameters : Element -> NodeDefinition
definitionFromElementParameters =
    Spec.definitionFromElementParameters


nameFromElementParameters : Element -> String
nameFromElementParameters ele =
    nameFromNodeDefinition (definitionFromElementParameters ele)


elementParameters : NodeDefinition -> List Attribute -> Set String -> Element
elementParameters =
    Spec.elementParameters


elementParametersWithAnnotations : Set String -> Element -> Element
elementParametersWithAnnotations =
    Spec.elementParametersWithAnnotations


elementParametersWithAttributes : List Attribute -> Element -> Element
elementParametersWithAttributes =
    Spec.elementParametersWithAttributes


elementParametersWithDefinition : NodeDefinition -> Element -> Element
elementParametersWithDefinition =
    Spec.elementParametersWithDefinition


blockNodeWithElementParameters : Element -> BlockNode -> BlockNode
blockNodeWithElementParameters parameters node =
    case node of
        BlockNode c ->
            BlockNode { c | parameters = parameters }


annotationsFromBlockNode : BlockNode -> Set String
annotationsFromBlockNode node =
    annotationsFromElementParameters <| elementParametersFromBlockNode node


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


elementParametersFromBlockNode : BlockNode -> Element
elementParametersFromBlockNode node =
    case node of
        BlockNode n ->
            n.parameters


comparableElementParameters : Element -> ( String, List Attribute, Set String )
comparableElementParameters p =
    ( nameFromElementParameters p
    , attributesFromElementParameters p
    , annotationsFromElementParameters p
    )


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


type InlineLeafParameters
    = InlineLeafParameters InlineLeafParametersContents


type alias InlineLeafParametersContents =
    { marks : List Mark
    , parameters : Element
    }


marksFromInlineLeafParameters : InlineLeafParameters -> List Mark
marksFromInlineLeafParameters parameters =
    case parameters of
        InlineLeafParameters c ->
            c.marks


elementParametersFromInlineLeafParameters : InlineLeafParameters -> Element
elementParametersFromInlineLeafParameters parameters =
    case parameters of
        InlineLeafParameters c ->
            c.parameters


inlineLeafParameters : Element -> List Mark -> InlineLeafParameters
inlineLeafParameters parameters marks =
    InlineLeafParameters { parameters = parameters, marks = marks }


inlineLeafParametersWithElementParameters : Element -> InlineLeafParameters -> InlineLeafParameters
inlineLeafParametersWithElementParameters eparams iparams =
    case iparams of
        InlineLeafParameters c ->
            InlineLeafParameters { c | parameters = eparams }


inlineLeafParametersWithMarks : List Mark -> InlineLeafParameters -> InlineLeafParameters
inlineLeafParametersWithMarks marks iparams =
    case iparams of
        InlineLeafParameters c ->
            InlineLeafParameters { c | marks = marks }


{-| An inline leaf node represents an inline element in your document. It can either be an inline
leaf node, like an image or line break, or a text node.
-}
type InlineLeaf
    = InlineLeaf InlineLeafParameters
    | TextLeaf TextLeafParameters


{-| TextNodeContents represents the attributes that can be in a text node. The core attributes
are marks and text.
-}
type TextLeafParameters
    = TextLeafParameters TextLeafParametersContents


emptyTextLeafParameters : TextLeafParameters
emptyTextLeafParameters =
    TextLeafParameters { text = "", marks = [], annotations = Set.empty }


withText : String -> TextLeafParameters -> TextLeafParameters
withText s parameters =
    case parameters of
        TextLeafParameters c ->
            TextLeafParameters { c | text = s }


textLeafParametersWithAnnotations : Set String -> TextLeafParameters -> TextLeafParameters
textLeafParametersWithAnnotations annotations parameters =
    case parameters of
        TextLeafParameters c ->
            TextLeafParameters { c | annotations = annotations }


textLeafParametersWithMarks : List Mark -> TextLeafParameters -> TextLeafParameters
textLeafParametersWithMarks marks parameters =
    case parameters of
        TextLeafParameters c ->
            TextLeafParameters { c | marks = marks }


textLeafWithText : String -> InlineLeaf
textLeafWithText s =
    TextLeaf (TextLeafParameters { text = s, marks = [], annotations = Set.empty })


marksFromTextLeafParameters : TextLeafParameters -> List Mark
marksFromTextLeafParameters parameters =
    case parameters of
        TextLeafParameters c ->
            c.marks


comparableMarksFromTextLeafParameters : TextLeafParameters -> List ( String, List Attribute )
comparableMarksFromTextLeafParameters parameters =
    List.map (\m -> ( name m, attributes m )) (marksFromTextLeafParameters parameters)


comparableMarksFromInlineLeafParameters : InlineLeafParameters -> List ( String, List Attribute )
comparableMarksFromInlineLeafParameters parameters =
    List.map (\m -> ( name m, attributes m )) (marksFromInlineLeafParameters parameters)


annotationsFromTextLeafParameters : TextLeafParameters -> Set String
annotationsFromTextLeafParameters parameters =
    case parameters of
        TextLeafParameters c ->
            c.annotations


text : TextLeafParameters -> String
text parameters =
    case parameters of
        TextLeafParameters c ->
            c.text


type alias TextLeafParametersContents =
    { marks : List Mark
    , annotations : Set String
    , text : String
    }


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
        InlineLeaf il1 ->
            case i2 of
                InlineLeaf il2 ->
                    comparableMarksFromInlineLeafParameters il1
                        == comparableMarksFromInlineLeafParameters il2
                        && comparableElementParameters (elementParametersFromInlineLeafParameters il1)
                        == comparableElementParameters (elementParametersFromInlineLeafParameters il2)

                _ ->
                    False

        TextLeaf tl1 ->
            case i2 of
                TextLeaf tl2 ->
                    comparableMarksFromTextLeafParameters tl1
                        == comparableMarksFromTextLeafParameters tl2
                        && text tl1
                        == text tl2

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
            comparableElementParameters <| elementParametersFromBlockNode bn1

        e2 =
            comparableElementParameters <| elementParametersFromBlockNode bn2
    in
    if e1 /= e2 then
        False

    else
        isSameChildNodes (childNodes bn1) (childNodes bn2)


inlineLeaf : Element -> List Mark -> InlineLeaf
inlineLeaf parameters mark =
    InlineLeaf (inlineLeafParameters parameters mark)


textLeaf : String -> List Mark -> InlineLeaf
textLeaf s marks =
    TextLeaf
        (emptyTextLeafParameters
            |> withText s
            |> textLeafParametersWithMarks marks
        )


parent : Path -> Path
parent path =
    List.take (List.length path - 1) path
