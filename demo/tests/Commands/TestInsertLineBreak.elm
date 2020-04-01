module Commands.TestInsertLineBreak exposing (..)

import Array
import Expect
import RichText.Commands exposing (insertLineBreak)
import RichText.Definitions exposing (doc, hardBreak, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline
        , block
        , blockChildren
        , inlineChildren
        , inlineElement
        , plainText
        )
import RichText.Model.Selection exposing (caret)
import RichText.Model.State exposing (State, state)
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
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


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
                                [ plainText "te"
                                , inlineElement (Element.element hardBreak []) []
                                , plainText "xt"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 2 ] 0)


testInsertLineBreak : Test
testInsertLineBreak =
    describe "Tests the insertLineBreak transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (insertLineBreak example)
        ]
