module RichTextEditor.Model.Spec exposing
    ( Spec
    , emptySpec
    , markDefinition
    , markDefinitions
    , nodeDefinition
    , nodeDefinitions
    , withMarkDefinitions
    , withNodeDefinitions
    )

import Dict exposing (Dict)
import RichTextEditor.Model.Internal
    exposing
        ( ContentType(..)
        , Element
        , ElementToHtml
        , HtmlToElement
        , HtmlToMark
        , Mark
        , MarkDefinition(..)
        , MarkToHtml
        , NodeDefinition(..)
        )
import RichTextEditor.Model.MarkDefinition as MarkDefinition
import RichTextEditor.Model.NodeDefinition as NodeDefinition


type Spec
    = Spec SpecContents


emptySpec =
    Spec { marks = [], nameToMark = Dict.empty, nodes = [], nameToNode = Dict.empty }


markDefinitions : Spec -> List MarkDefinition
markDefinitions spec =
    case spec of
        Spec c ->
            c.marks


nodeDefinitions : Spec -> List NodeDefinition
nodeDefinitions spec =
    case spec of
        Spec c ->
            c.nodes


withMarkDefinitions : List MarkDefinition -> Spec -> Spec
withMarkDefinitions marks spec =
    case spec of
        Spec c ->
            Spec
                { c
                    | marks = marks
                    , nameToMark = Dict.fromList <| List.map (\x -> ( MarkDefinition.name x, x )) marks
                }


withNodeDefinitions : List NodeDefinition -> Spec -> Spec
withNodeDefinitions nodes spec =
    case spec of
        Spec c ->
            Spec
                { c
                    | nodes = nodes
                    , nameToNode = Dict.fromList <| List.map (\x -> ( NodeDefinition.name x, x )) nodes
                }


markDefinition : String -> Spec -> Maybe MarkDefinition
markDefinition name spec =
    case spec of
        Spec c ->
            Dict.get name c.nameToMark


nodeDefinition : String -> Spec -> Maybe NodeDefinition
nodeDefinition name spec =
    case spec of
        Spec c ->
            Dict.get name c.nameToNode


type alias SpecContents =
    { marks : List MarkDefinition
    , nameToMark : Dict String MarkDefinition
    , nodes : List NodeDefinition
    , nameToNode : Dict String NodeDefinition
    }
