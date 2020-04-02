module Commands.TestLiftEmpty exposing (..)

import Array
import Expect
import RichText.Commands exposing (liftEmpty)
import RichText.Definitions exposing (blockquote, doc, paragraph)
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
import RichText.Model.Selection exposing (caret, range)
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
                        (Element.element blockquote [])
                        (blockChildren <|
                            Array.fromList
                                [ block (Element.element paragraph [])
                                    (inlineChildren <| Array.fromList [ plainText "" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0, 0 ] 0)


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


nonEmptyExample : State
nonEmptyExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element blockquote [])
                        (blockChildren <|
                            Array.fromList
                                [ block (Element.element paragraph [])
                                    (inlineChildren <| Array.fromList [ plainText "test" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0, 0 ] 0)


testLiftEmpty : Test
testLiftEmpty =
    describe "Tests the liftEmpty transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (liftEmpty example)
        , test "it fails if it's not empty" <|
            \_ ->
                Expect.equal (Err "I can only lift an empty text block") (liftEmpty nonEmptyExample)
        ]
