module RichTextEditor.Internal.HtmlNode exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.Element as Element exposing (Element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.InlineElement exposing (element)
import RichTextEditor.Model.Mark as Mark exposing (Mark)
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , ChildNodes(..)
        , InlineLeaf(..)
        , InlineLeafTree(..)
        , childNodes
        , elementFromBlockNode
        , fromBlockArray
        , fromInlineArray
        , treeFromInlineArray
        )
import RichTextEditor.Model.Spec
    exposing
        ( Spec
        , toHtmlNodeFromMarkDefinition
        , toHtmlNodeFromNodeDefinition
        )
import RichTextEditor.Model.Text exposing (text)


{-| Renders marks to their HtmlNode representation.
-}
markToHtmlNode : Mark -> Array HtmlNode -> HtmlNode
markToHtmlNode mark children =
    let
        markDefinition =
            Mark.definition mark
    in
    toHtmlNodeFromMarkDefinition markDefinition mark children


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : Element -> Array HtmlNode -> HtmlNode
elementToHtmlNode parameters children =
    let
        nodeDefinition =
            Element.definition parameters
    in
    toHtmlNodeFromNodeDefinition nodeDefinition parameters children


{-| Renders element block nodes to their HtmlNode representation.
-}
editorBlockNodeToHtmlNode : BlockNode -> HtmlNode
editorBlockNodeToHtmlNode node =
    elementToHtmlNode (elementFromBlockNode node) (childNodesToHtmlNode (childNodes node))


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

        ElementLeaf l ->
            elementToHtmlNode (element l) Array.empty
