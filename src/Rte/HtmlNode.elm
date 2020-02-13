module Rte.HtmlNode exposing (..)

import Array exposing (Array)
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, Spec, TextNodeContents)
import Rte.Spec exposing (findMarkDefinitionsFromSpec, findNodeDefinitionFromSpec)


{-| Renders marks to their HtmlNode representation.
-}
marksToHtmlNode : Spec -> List Mark -> HtmlNode -> HtmlNode
marksToHtmlNode spec marks node =
    let
        marksAndDefinitions =
            findMarkDefinitionsFromSpec marks spec
    in
    List.foldr
        (\( mark, markDefinition ) htmlNode -> markDefinition.toHtmlNode mark (Array.fromList [ htmlNode ]))
        node
        marksAndDefinitions


{-| Renders element parameters to their HtmlNode representation.
-}
elementToHtmlNode : Spec -> ElementParameters -> Array HtmlNode -> HtmlNode
elementToHtmlNode spec parameters children =
    let
        nodeDefinition =
            findNodeDefinitionFromSpec parameters.name spec

        renderedNode =
            nodeDefinition.toHtmlNode parameters children
    in
    marksToHtmlNode spec parameters.marks renderedNode


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
            Array.map (editorInlineLeafToHtmlNode spec) inlineLeafArray

        Leaf ->
            Array.empty


{-| Renders text nodes to their HtmlNode representation.
-}
textToHtmlNode : Spec -> TextNodeContents -> HtmlNode
textToHtmlNode spec contents =
    marksToHtmlNode spec contents.marks (TextNode contents.text)


{-| Renders inline leaf nodes to their HtmlNode representation.
-}
editorInlineLeafToHtmlNode : Spec -> EditorInlineLeaf -> HtmlNode
editorInlineLeafToHtmlNode spec node =
    case node of
        TextLeaf contents ->
            textToHtmlNode spec contents

        InlineLeaf parameters ->
            elementToHtmlNode spec parameters Array.empty
