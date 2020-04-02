module Commands.TestWrap exposing (..)

import Array
import Expect
import RichText.Commands exposing (wrap)
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
        , markedText
        , plainText
        )
import RichText.Model.Selection exposing (caret, singleNodeRange)
import RichText.Model.State exposing (State, state)
import Test exposing (Test, describe, test)


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element blockquote [])
                        (blockChildren <|
                            Array.fromList
                                [ block
                                    (Element.element paragraph [])
                                    (inlineChildren <| Array.fromList [ plainText "text" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0, 0 ] 0)


testWrap : Test
testWrap =
    describe "Tests the toggleBlock transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (wrap identity (Element.element blockquote []) example)
        ]
