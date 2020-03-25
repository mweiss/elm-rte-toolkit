module RichText.Internal.HtmlNode exposing (..)

import Array exposing (Array)
import RichText.Config.MarkDefinition as MarkDefinition
import RichText.Config.NodeDefinition as NodeDefinition
import RichText.Config.Spec exposing (Spec)
import RichText.Internal.Spec exposing (markDefinitionWithDefault, nodeDefinitionWithDefault)
import RichText.Model.Element exposing (Element)
import RichText.Model.HtmlNode exposing (HtmlNode(..))
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark exposing (Mark)
import RichText.Model.Node as Node
    exposing
        ( Block
        , Children(..)
        , Inline(..)
        , InlineTree(..)
        , childNodes
        , toBlockArray
        , toInlineArray
        , toInlineTree
        )
import RichText.Model.Text exposing (text)


childNodesPlaceholder =
    Array.fromList
        [ ElementNode "__child_node_marker__" [] Array.empty ]


{-| Renders marks to their HtmlNode representation.
-}
markToHtmlNode : Spec -> Mark -> Array HtmlNode -> HtmlNode
markToHtmlNode spec mark children =
    let
        markDefinition =
            markDefinitionWithDefault mark spec
    in
    MarkDefinition.toHtmlNode markDefinition mark children


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : Spec -> Element -> Array HtmlNode -> HtmlNode
elementToHtmlNode spec parameters children =
    let
        nodeDefinition =
            nodeDefinitionWithDefault parameters spec
    in
    NodeDefinition.toHtmlNode nodeDefinition parameters children


{-| Renders element block nodes to their HtmlNode representation.
-}
editorBlockNodeToHtmlNode : Spec -> Block -> HtmlNode
editorBlockNodeToHtmlNode spec node =
    elementToHtmlNode spec (Node.element node) (childNodesToHtmlNode spec (childNodes node))


{-| Renders child nodes to their HtmlNode representation.
-}
childNodesToHtmlNode : Spec -> Children -> Array HtmlNode
childNodesToHtmlNode spec childNodes =
    case childNodes of
        BlockChildren blockArray ->
            Array.map (editorBlockNodeToHtmlNode spec) (toBlockArray blockArray)

        InlineChildren inlineLeafArray ->
            Array.map (editorInlineLeafTreeToHtmlNode spec (toInlineArray inlineLeafArray)) (toInlineTree inlineLeafArray)

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


editorInlineLeafTreeToHtmlNode : Spec -> Array Inline -> InlineTree -> HtmlNode
editorInlineLeafTreeToHtmlNode spec array tree =
    case tree of
        LeafNode i ->
            case Array.get i array of
                Nothing ->
                    errorNode

                Just l ->
                    editorInlineLeafToHtmlNode spec l

        MarkNode n ->
            markToHtmlNode spec n.mark (Array.map (editorInlineLeafTreeToHtmlNode spec array) n.children)


{-| Renders inline leaf nodes to their HtmlNode representation.
-}
editorInlineLeafToHtmlNode : Spec -> Inline -> HtmlNode
editorInlineLeafToHtmlNode spec node =
    case node of
        Text contents ->
            textToHtmlNode (text contents)

        InlineElement l ->
            elementToHtmlNode spec (InlineElement.element l) Array.empty
