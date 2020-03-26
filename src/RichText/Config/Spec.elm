module RichText.Config.Spec exposing (Spec, emptySpec, markDefinitions, markDefinition, nodeDefinitions, nodeDefinition, withMarkDefinitions, withNodeDefinitions)

{-| A spec describes what nodes and marks can be in an editor.

@docs Spec, emptySpec, markDefinitions, markDefinition, nodeDefinitions, nodeDefinition, withMarkDefinitions, withNodeDefinitions

-}

import Dict exposing (Dict)
import RichText.Internal.Definitions
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


{-| A spec describes what nodes and marks can be in an editor. It's used internally to encode an
editor to html, and to transform html to editor nodes. Note for the latter, the order of nodes and
marks is significant, because that is the order in which each node and mark's decoder method is
applied.

    simpleSpec : Spec
    simpleSpec =
        emptySpec
            |> withNodeDefinitions
                [ codeBlock
                , crazyBlock
                , paragraph
                , image
                ]
            |> withMarkDefinitions
                [ bold
                , italic
                ]

-}
type Spec
    = Spec SpecContents


{-| An empty spec
-}
emptySpec : Spec
emptySpec =
    Spec { marks = [], nameToMark = Dict.empty, nodes = [], nameToNode = Dict.empty }


{-| list of `MarkDefinition` from a spec
-}
markDefinitions : Spec -> List MarkDefinition
markDefinitions spec =
    case spec of
        Spec c ->
            c.marks


{-| list of `NodeDefinition` from a spec
-}
nodeDefinitions : Spec -> List NodeDefinition
nodeDefinitions spec =
    case spec of
        Spec c ->
            c.nodes


{-| a spec with the given mark definitions
-}
withMarkDefinitions : List MarkDefinition -> Spec -> Spec
withMarkDefinitions marks spec =
    case spec of
        Spec c ->
            Spec
                { c
                    | marks = marks
                    , nameToMark =
                        Dict.fromList <|
                            List.map
                                (\x ->
                                    case x of
                                        MarkDefinition m ->
                                            ( m.name, x )
                                )
                                marks
                }


{-| a spec with the given node definitions
-}
withNodeDefinitions : List NodeDefinition -> Spec -> Spec
withNodeDefinitions nodes spec =
    case spec of
        Spec c ->
            Spec
                { c
                    | nodes = nodes
                    , nameToNode =
                        Dict.fromList <|
                            List.map
                                (\x ->
                                    case x of
                                        NodeDefinition m ->
                                            ( m.name, x )
                                )
                                nodes
                }


{-| Returns the mark definition with the given name from a spec.

    markDefinition "bold" markdown
    --> Just (bold mark definition)

-}
markDefinition : String -> Spec -> Maybe MarkDefinition
markDefinition name spec =
    case spec of
        Spec c ->
            Dict.get name c.nameToMark


{-| Returns the node definition with the given name from a spec.

    nodeDefinition "paragraph" markdown
    --> Just (paragraph node definition)

-}
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
