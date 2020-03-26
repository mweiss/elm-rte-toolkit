module RichText.Config.Spec exposing (Spec, emptySpec, markDefinitions, markDefinition, elementDefinitions, elementDefinition, withMarkDefinitions, withElementDefinitions)

{-| A spec describes what nodes and marks can be in an editor.

@docs Spec, emptySpec, markDefinitions, markDefinition, elementDefinitions, elementDefinition, withMarkDefinitions, withElementDefinitions

-}

import Dict exposing (Dict)
import RichText.Internal.Definitions
    exposing
        ( ContentType(..)
        , Element
        , ElementDefinition(..)
        , ElementToHtml
        , HtmlToElement
        , HtmlToMark
        , Mark
        , MarkDefinition(..)
        , MarkToHtml
        )


{-| A spec describes what nodes and marks can be in an editor. It's used internally to encode an
editor to html, and to transform html to editor nodes. Note for the latter, the order of nodes and
marks is significant, because that is the order in which each node and mark's decoder method is
applied.

    simpleSpec : Spec
    simpleSpec =
        emptySpec
            |> withElementDefinitions
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
    Spec { marks = [], nameToMark = Dict.empty, elements = [], nameToElement = Dict.empty }


{-| list of `MarkDefinition` from a spec
-}
markDefinitions : Spec -> List MarkDefinition
markDefinitions spec =
    case spec of
        Spec c ->
            c.marks


{-| list of `ElementDefinition` from a spec
-}
elementDefinitions : Spec -> List ElementDefinition
elementDefinitions spec =
    case spec of
        Spec c ->
            c.elements


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


{-| a spec with the given element definitions
-}
withElementDefinitions : List ElementDefinition -> Spec -> Spec
withElementDefinitions nodes spec =
    case spec of
        Spec c ->
            Spec
                { c
                    | elements = nodes
                    , nameToElement =
                        Dict.fromList <|
                            List.map
                                (\x ->
                                    case x of
                                        ElementDefinition m ->
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


{-| Returns the element definition with the given name from a spec.

    elementDefinition "paragraph" markdown
    --> Just (paragraph element definition)

-}
elementDefinition : String -> Spec -> Maybe ElementDefinition
elementDefinition name spec =
    case spec of
        Spec c ->
            Dict.get name c.nameToElement


type alias SpecContents =
    { marks : List MarkDefinition
    , nameToMark : Dict String MarkDefinition
    , elements : List ElementDefinition
    , nameToElement : Dict String ElementDefinition
    }
