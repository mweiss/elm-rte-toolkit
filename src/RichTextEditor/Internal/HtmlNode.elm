module RichTextEditor.Internal.HtmlNode exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , ElementParameters
        , InlineLeaf(..)
        , InlineLeafTree(..)
        , childNodes
        , definitionFromElementParameters
        , elementParametersFromBlockNode
        , elementParametersFromInlineLeafParameters
        , fromBlockArray
        , fromInlineArray
        , text
        , treeFromInlineArray
        )
import RichTextEditor.Model.Spec
    exposing
        ( Spec
        , definitionFromMark
        , toHtmlNodeFromMarkDefinition
        , toHtmlNodeFromNodeDefinition
        )


{-| Renders marks to their HtmlNode representation.
-}
markToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
markToHtmlNode mark children =
    let
        markDefinition =
            definitionFromMark mark
    in
    toHtmlNodeFromMarkDefinition markDefinition mark children


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
elementToHtmlNode parameters children =
    let
        nodeDefinition =
            definitionFromElementParameters parameters
    in
    toHtmlNodeFromNodeDefinition nodeDefinition parameters children


{-| Renders element block nodes to their HtmlNode representation.
-}
editorBlockNodeToHtmlNode : BlockNode -> HtmlNode
editorBlockNodeToHtmlNode node =
    elementToHtmlNode (elementParametersFromBlockNode node) (childNodesToHtmlNode (childNodes node))


{-| Renders child nodes to their HtmlNode representation.
-}
childNodesToHtmlNode : ChildNodes -> Array HtmlNode
childNodesToHtmlNode childNodes =
    case childNodes of
        BlockChildren blockArray ->
            Array.map editorBlockNodeToHtmlNode (fromBlockArray blockArray)

        InlineChildren inlineLeafArray ->
            Array.map (editorInlineLeafTreeToHtmlNode (fromInlineArray inlineLeafArray)) (treeFromInlineArray inlineLeafArray)

        Leaf ->
            Array.empty


{-| Renders text nodes to their HtmlNode representation.
-}
textToHtmlNode : String -> HtmlNode
textToHtmlNode text =
    TextNode text


errorNode : HtmlNode
errorNode =
    ElementNode "div" [ ( "class", "rte-error" ) ] Array.empty


editorInlineLeafTreeToHtmlNode : Array InlineLeaf -> InlineLeafTree -> HtmlNode
editorInlineLeafTreeToHtmlNode array tree =
    case tree of
        LeafNode i ->
            case Array.get i array of
                Nothing ->
                    errorNode

                Just l ->
                    editorInlineLeafToHtmlNode l

        MarkNode n ->
            markToHtmlNode n.mark (Array.map (editorInlineLeafTreeToHtmlNode array) n.children)


{-| Renders inline leaf nodes to their HtmlNode representation.
-}
editorInlineLeafToHtmlNode : InlineLeaf -> HtmlNode
editorInlineLeafToHtmlNode node =
    case node of
        TextLeaf contents ->
            textToHtmlNode (text contents)

        InlineLeaf l ->
            elementToHtmlNode (elementParametersFromInlineLeafParameters l) Array.empty
