module RichTextEditor.Model.Decorations exposing
    ( Decorations
    , ElementDecoratorFunction
    , MarkDecoratorFunction
    , addElementDecoration
    , addMarkDecoration
    , elementDecorators
    , emptyDecorations
    , markDecorators
    , withElementDecorators
    , withMarkDecorators
    )

import Dict exposing (Dict)
import Html
import RichTextEditor.Model.Mark exposing (Mark)
import RichTextEditor.Model.Node exposing (Element, Path)


type Decorations msg
    = Decorations (DecorationsContents msg)


type alias DecorationsContents msg =
    { marks : Dict String (List (MarkDecoratorFunction msg))
    , elements : Dict String (List (ElementDecoratorFunction msg))
    }


emptyDecorations : Decorations msg
emptyDecorations =
    Decorations { marks = Dict.empty, elements = Dict.empty }


markDecorators : Decorations msg -> Dict String (List (MarkDecoratorFunction msg))
markDecorators d =
    case d of
        Decorations c ->
            c.marks


elementDecorators : Decorations msg -> Dict String (List (ElementDecoratorFunction msg))
elementDecorators d =
    case d of
        Decorations c ->
            c.elements


withMarkDecorators : Dict String (List (MarkDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withMarkDecorators marks d =
    case d of
        Decorations c ->
            Decorations { c | marks = marks }


withElementDecorators : Dict String (List (ElementDecoratorFunction msg)) -> Decorations msg -> Decorations msg
withElementDecorators elements d =
    case d of
        Decorations c ->
            Decorations { c | elements = elements }


type alias ElementDecoratorFunction msg =
    Path -> Element -> Path -> List (Html.Attribute msg)


type alias MarkDecoratorFunction msg =
    Path -> Mark -> Path -> List (Html.Attribute msg)


addElementDecoration : String -> ElementDecoratorFunction msg -> Decorations msg -> Decorations msg
addElementDecoration name decorator decorations =
    let
        eleDecorators =
            elementDecorators decorations

        previousDecorations =
            Maybe.withDefault [] (Dict.get name eleDecorators)
    in
    decorations
        |> withElementDecorators (Dict.insert name (decorator :: previousDecorations) eleDecorators)


addMarkDecoration : String -> MarkDecoratorFunction msg -> Decorations msg -> Decorations msg
addMarkDecoration name decorator decorations =
    let
        mDecorators =
            markDecorators decorations

        previousDecorations =
            Maybe.withDefault [] (Dict.get name mDecorators)
    in
    decorations
        |> withMarkDecorators (Dict.insert name (decorator :: previousDecorations) mDecorators)
