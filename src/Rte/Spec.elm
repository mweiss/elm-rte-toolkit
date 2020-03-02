module Rte.Spec exposing (..)

import Array exposing (Array)
import List.Extra
import Rte.Model exposing (EditorAttribute(..), ElementParameters, HtmlNode(..), Mark, MarkDefinition, NodeDefinition, Spec)


emptySpec : Spec
emptySpec =
    { nodes = [], marks = [] }


childNodesPlaceholder =
    Array.fromList
        [ ElementNode "__child_node_marker__" [] Array.empty ]


defaultElementToHtml : ElementParameters -> Array HtmlNode -> HtmlNode
defaultElementToHtml elementParameters children =
    ElementNode elementParameters.name
        (List.map
            (\attr ->
                case attr of
                    StringAttribute k v ->
                        ( k, v )
            )
            elementParameters.attributes
        )
        children


defaultMarkToHtml : Mark -> Array HtmlNode -> HtmlNode
defaultMarkToHtml mark children =
    ElementNode "span"
        (List.map
            (\attr ->
                case attr of
                    StringAttribute k v ->
                        ( k, v )
            )
            mark.attributes
        )
        children


findNodeDefinitionFromSpec : String -> Spec -> NodeDefinition
findNodeDefinitionFromSpec name spec =
    Maybe.withDefault { name = name, toHtmlNode = defaultElementToHtml } (List.Extra.find (\n -> n.name == name) spec.nodes)


findMarkDefinitionFromSpec : String -> Spec -> MarkDefinition
findMarkDefinitionFromSpec name spec =
    Maybe.withDefault { name = name, toHtmlNode = defaultMarkToHtml } (List.Extra.find (\n -> n.name == name) spec.marks)


findMarkDefinitionsFromSpec : List Mark -> Spec -> List ( Mark, MarkDefinition )
findMarkDefinitionsFromSpec marks spec =
    List.map
        (\m ->
            ( m, findMarkDefinitionFromSpec m.name spec )
        )
        marks
