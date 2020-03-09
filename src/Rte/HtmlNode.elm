module Rte.HtmlNode exposing (..)

import Array exposing (Array)
import Rte.Model
    exposing
        ( ChildNodes(..)
        , EditorBlockNode
        , EditorInlineLeaf(..)
        , ElementParameters
        , HtmlNode(..)
        , InlineLeafTree(..)
        , Mark
        , Spec
        , TextLeafContents
        )
import Rte.Spec
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
            findMarkDefinitionFromSpecWithDefault mark.name spec
    in
    markDefinition.toHtmlNode mark children


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : Spec -> ElementParameters -> Array HtmlNode -> HtmlNode
elementToHtmlNode spec parameters children =
    let
        nodeDefinition =
            findNodeDefinitionFromSpecWithDefault parameters.name spec
    in
    nodeDefinition.toHtmlNode parameters children


{-| Renders element block nodes to their HtmlNode representation.
-}
editorBlockNodeToHtmlNode : Spec -> EditorBlockNode -> HtmlNode
editorBlockNodeToHtmlNode spec node =
    elementToHtmlNode spec node.parameters (childNodesToHtmlNode spec node.childNodes)


{-| Renders child nodes to their HtmlNode representation.
-}
childNodesToHtmlNode : Spec -> ChildNodes -> Array HtmlNode
childNodesToHtmlNode spec childNodes =
    case childNodes of
        BlockArray blockArray ->
            Array.map (editorBlockNodeToHtmlNode spec) blockArray

        InlineLeafArray inlineLeafArray ->
            Array.map (editorInlineLeafTreeToHtmlNode spec inlineLeafArray.array) inlineLeafArray.tree

        Leaf ->
            Array.empty


{-| Renders text nodes to their HtmlNode representation.
-}
textToHtmlNode : TextLeafContents -> HtmlNode
textToHtmlNode contents =
    TextNode contents.text


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
            textToHtmlNode contents

        InlineLeaf l ->
            elementToHtmlNode spec l.parameters Array.empty
