module Rte.Decorations exposing (..)

import Dict
import Html
import Html.Attributes
import Html.Events
import Rte.Model exposing (..)
import Rte.Selection exposing (caretSelection)


emptyDecorations : Decorations msg
emptyDecorations =
    { marks = Dict.empty, elements = Dict.empty }


selectableDecoration : DecoderFunc msg -> NodePath -> ElementParameters -> NodePath -> List (Html.Attribute msg)
selectableDecoration decoder editorNodePath elementParameters relativeHtmlNodePath =
    (if List.member selectionMark elementParameters.marks then
        [ Html.Attributes.class "rte-selected" ]

     else
        []
    )
        ++ [ Html.Events.onClick (decoder <| SelectionEvent (Just (caretSelection editorNodePath 0)) False) ]


addElementDecoration : String -> ElementDecoratorFunction msg -> Decorations msg -> Decorations msg
addElementDecoration name decorator decorations =
    let
        previousDecorations =
            Maybe.withDefault [] (Dict.get name decorations.elements)
    in
    { decorations | elements = Dict.insert name (decorator :: previousDecorations) decorations.elements }


addMarkDecoration : String -> MarkDecoratorFunction msg -> Decorations msg -> Decorations msg
addMarkDecoration name decorator decorations =
    let
        previousDecorations =
            Maybe.withDefault [] (Dict.get name decorations.marks)
    in
    { decorations | marks = Dict.insert name (decorator :: previousDecorations) decorations.marks }


getMarkDecorators : String -> Decorations msg -> List (MarkDecoratorFunction msg)
getMarkDecorators name decorations =
    Maybe.withDefault [] (Dict.get name decorations.marks)


getElementDecorators : String -> Decorations msg -> List (ElementDecoratorFunction msg)
getElementDecorators name decorations =
    Maybe.withDefault [] (Dict.get name decorations.elements)
