module Commands.TestDeleteInlineElement exposing (..)

import Array
import Expect
import RichText.Commands exposing (deleteInlineElement)
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
        (Just <| caret [ 0, 0 ] 4)


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
        (Just <| caret [ 0, 0 ] 4)


testDeleteInlineElement : Test
testDeleteInlineElement =
    describe "Tests the deleteInlineElement transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (deleteInlineElement example)
        , test "Tests that we can only delete an inline element when the offset is not at the end of a text node" <|
            \_ ->
                Expect.equal (Err "I cannot delete an inline element if the cursor is not at the end of an inline node")
                    (deleteInlineElement (example |> withSelection (Just <| caret [ 0, 0 ] 1)))
        ]
