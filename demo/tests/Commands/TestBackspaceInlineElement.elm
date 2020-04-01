module Commands.TestBackspaceInlineElement exposing (..)

import Array
import Expect
import RichText.Commands exposing (backspaceInlineElement)
import RichText.Definitions exposing (doc, image, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Node exposing (Block, Children(..), Inline, block, blockChildren, inlineChildren, inlineElement, plainText)
import RichText.Model.Selection exposing (caret)
import RichText.Model.State exposing (State, state, withSelection)
import Test exposing (Test, describe, test)


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text"
                                , inlineElement (Element.element image []) []
                                , plainText "text2"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 2 ] 0)


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text"
                                , plainText "text2"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


testBackspaceInlineElement : Test
testBackspaceInlineElement =
    describe "Tests the backspaceInlineElement transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (backspaceInlineElement example)
        , test "Tests that we can only backspace an inline element when the offset is 0" <|
            \_ ->
                Expect.equal (Err "I can only backspace an inline element if the offset is 0")
                    (backspaceInlineElement (example |> withSelection (Just <| caret [ 0, 2 ] 1)))
        ]
