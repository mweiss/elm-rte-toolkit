module RichTextEditor.Model.Spec exposing
    ( Spec
    , emptySpec
    , markDefinition
    , markDefinitionWithDefault
    , markDefinitions
    , nodeDefinition
    , nodeDefinitionWithDefault
    , nodeDefinitions
    , withMarkDefinitions
    , withNodeDefinitions
    )

import Dict exposing (Dict)
import RichTextEditor.Model.Internal.Model
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
        , nameFromElement
        , nameFromMark
        )
import RichTextEditor.Model.MarkDefinition as MarkDefinition exposing (defaultMarkDefinition)
import RichTextEditor.Model.NodeDefinition as NodeDefinition exposing (defaultNodeDefinition)


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


markDefinitionWithDefault : Mark -> Spec -> MarkDefinition
markDefinitionWithDefault mark spec =
    let
        name =
            nameFromMark mark
    in
    Maybe.withDefault (defaultMarkDefinition name) (markDefinition name spec)


nodeDefinitionWithDefault : Element -> Spec -> NodeDefinition
nodeDefinitionWithDefault ele spec =
    let
        name =
            nameFromElement ele
    in
    Maybe.withDefault (defaultNodeDefinition name) (nodeDefinition name spec)
