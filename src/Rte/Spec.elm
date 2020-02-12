module Rte.Spec exposing (..)

import List.Extra
import Rte.Model exposing (EditorAttribute(..), ElementParameters, HtmlNode(..), Mark, MarkDefinition, NodeDefinition, Spec)


emptySpec : Spec
emptySpec =
    { nodes = [], marks = [] }


childNodesPlaceholder =
    [ ElementNode "__child_node_marker__" [] [] ]


defaultToHtml : ElementParameters -> List HtmlNode -> HtmlNode
defaultToHtml elementParameters children =
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


findNodeDefinitionFromSpec : String -> Spec -> NodeDefinition
findNodeDefinitionFromSpec name spec =
    Maybe.withDefault { name = name, toHtmlNode = defaultToHtml } (List.Extra.find (\n -> n.name == name) spec.nodes)


findMarkDefinitionFromSpec : String -> Spec -> Maybe MarkDefinition
findMarkDefinitionFromSpec name spec =
    List.Extra.find (\n -> n.name == name) spec.marks


findMarkDefinitionsFromSpec : List Mark -> Spec -> List ( Mark, MarkDefinition )
findMarkDefinitionsFromSpec marks spec =
    List.concatMap
        (\m ->
            case findMarkDefinitionFromSpec m.name spec of
                Nothing ->
                    []

                Just d ->
                    [ ( m, d ) ]
        )
        marks
