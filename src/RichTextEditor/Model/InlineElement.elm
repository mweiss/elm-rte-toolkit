module RichTextEditor.Model.InlineElement exposing
    ( InlineElement
    , InlineElementContents
    , element
    , inlineElement
    , marks
    , withElement
    , withMarks
    )

import RichTextEditor.Model.Element exposing (Element)
import RichTextEditor.Model.Mark exposing (Mark)


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
