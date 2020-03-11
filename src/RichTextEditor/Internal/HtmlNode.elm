module RichTextEditor.Internal.HtmlNode exposing (..)

import Array exposing (Array)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Mark exposing (Mark, name)
import RichTextEditor.Model.Node exposing (BlockNode, ChildNodes(..), EditorInlineLeaf(..), ElementParameters, InlineLeafTree(..), childNodes, elementParametersFromBlockNode, elementParametersFromInlineLeafParameters, fromBlockArray, fromInlineArray, nameFromElementParameters, text, treeFromInlineArray)
import RichTextEditor.Model.Spec exposing (Spec, toHtmlNodeFromMarkDefinition, toHtmlNodeFromNodeDefinition)
import RichTextEditor.Spec
    exposing
        ( findMarkDefinitionFromSpecWithDefault
        , findNodeDefinitionFromSpecWithDefault
        )


{-| Renders marks to their HtmlNode representation.
-}
markToHtmlNode : Spec -> Mark -> Array HtmlNode -> HtmlNode
markToHtmlNode spec mark children =
    let
        markDefinition =
            findMarkDefinitionFromSpecWithDefault (name mark) spec
    in
    toHtmlNodeFromMarkDefinition markDefinition mark children


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : Spec -> ElementParameters -> Array HtmlNode -> HtmlNode
elementToHtmlNode spec parameters children =
    let
        nodeDefinition =
            findNodeDefinitionFromSpecWithDefault (nameFromElementParameters parameters) spec
    in
    toHtmlNodeFromNodeDefinition nodeDefinition parameters children


{-| Renders element block nodes to their HtmlNode representation.
-}
editorBlockNodeToHtmlNode : Spec -> BlockNode -> HtmlNode
editorBlockNodeToHtmlNode spec node =
    elementToHtmlNode spec (elementParametersFromBlockNode node) (childNodesToHtmlNode spec (childNodes node))


{-| Renders child nodes to their HtmlNode representation.
-}
childNodesToHtmlNode : Spec -> ChildNodes -> Array HtmlNode
childNodesToHtmlNode spec childNodes =
    case childNodes of
        BlockChildren blockArray ->
            Array.map (editorBlockNodeToHtmlNode spec) (fromBlockArray blockArray)

        InlineChildren inlineLeafArray ->
            Array.map (editorInlineLeafTreeToHtmlNode spec (fromInlineArray inlineLeafArray)) (treeFromInlineArray inlineLeafArray)

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


editorInlineLeafTreeToHtmlNode : Spec -> Array EditorInlineLeaf -> InlineLeafTree -> HtmlNode
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
editorInlineLeafToHtmlNode : Spec -> EditorInlineLeaf -> HtmlNode
editorInlineLeafToHtmlNode spec node =
    case node of
        TextLeaf contents ->
            textToHtmlNode (text contents)

        InlineLeaf l ->
            elementToHtmlNode spec (elementParametersFromInlineLeafParameters l) Array.empty
