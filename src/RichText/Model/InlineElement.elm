module RichText.Model.InlineElement exposing (InlineElement, inlineElement, element, marks, withElement, withMarks)

{-| An inline element is an element with marks. It represents the contents of an inline node that is
not a text node.

@docs InlineElement, inlineElement, element, marks, withElement, withMarks

-}

import RichText.Model.Element exposing (Element)
import RichText.Model.Mark exposing (Mark)


{-| `InlineElement` is an element with marks. It represents the contents of an inline node that is
not a text node.
-}
type InlineElement
    = InlineElement InlineElementContents


type alias InlineElementContents =
    { marks : List Mark
    , element : Element
    }


{-| Marks from an inline element
-}
marks : InlineElement -> List Mark
marks parameters =
    case parameters of
        InlineElement c ->
            c.marks


{-| `Element` from an inline element
-}
element : InlineElement -> Element
element parameters =
    case parameters of
        InlineElement c ->
            c.element


{-| Creates an inline element from an element and a list of marks
-}
inlineElement : Element -> List Mark -> InlineElement
inlineElement parameters m =
    InlineElement { element = parameters, marks = m }


{-| Creates an inline element with the new `Element`
-}
withElement : Element -> InlineElement -> InlineElement
withElement eparams iparams =
    case iparams of
        InlineElement c ->
            InlineElement { c | element = eparams }


{-| Creates an inline element with the new marks
-}
withMarks : List Mark -> InlineElement -> InlineElement
withMarks m iparams =
    case iparams of
        InlineElement c ->
            InlineElement { c | marks = m }
