module RichTextEditor.Model.Spec exposing
    ( Spec
    , emptySpec
    , markDefinitions
    , nodeDefinitions
    , withMarkDefinitions
    , withNodeDefinitions
    )

import RichTextEditor.Model.Internal.Spec
    exposing
        ( ContentType(..)
        , ElementToHtml
        , HtmlToElement
        , HtmlToMark
        , MarkDefinition(..)
        , MarkToHtml
        , NodeDefinition(..)
        )


type Spec
    = Spec SpecContents


emptySpec =
    Spec { marks = [], nodes = [] }


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
            Spec { c | marks = marks }


withNodeDefinitions : List NodeDefinition -> Spec -> Spec
withNodeDefinitions nodes spec =
    case spec of
        Spec c ->
            Spec { c | nodes = nodes }


type alias SpecContents =
    { marks : List MarkDefinition
    , nodes : List NodeDefinition
    }
