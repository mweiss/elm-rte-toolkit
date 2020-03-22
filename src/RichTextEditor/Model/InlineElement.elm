module RichTextEditor.Model.InlineElement exposing
    ( InlineElement
    , InlineElementContents
    , comparableMarks
    , element
    , inlineElement
    , marks
    , withElement
    , withMarks
    )

import RichTextEditor.Model.Attribute exposing (Attribute)
import RichTextEditor.Model.Element exposing (Element)
import RichTextEditor.Model.Mark exposing (Mark, attributes, name)


type InlineElement
    = InlineElement InlineElementContents


type alias InlineElementContents =
    { marks : List Mark
    , element : Element
    }


marks : InlineElement -> List Mark
marks parameters =
    case parameters of
        InlineElement c ->
            c.marks


element : InlineElement -> Element
element parameters =
    case parameters of
        InlineElement c ->
            c.element


inlineElement : Element -> List Mark -> InlineElement
inlineElement parameters m =
    InlineElement { element = parameters, marks = m }


withElement : Element -> InlineElement -> InlineElement
withElement eparams iparams =
    case iparams of
        InlineElement c ->
            InlineElement { c | element = eparams }


withMarks : List Mark -> InlineElement -> InlineElement
withMarks m iparams =
    case iparams of
        InlineElement c ->
            InlineElement { c | marks = m }


comparableMarks : InlineElement -> List ( String, List Attribute )
comparableMarks parameters =
    List.map (\m -> ( name m, attributes m )) (marks parameters)
