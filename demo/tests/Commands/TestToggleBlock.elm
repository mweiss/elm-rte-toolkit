module Commands.TestToggleBlock exposing (..)

import Array
import Expect
import RichText.Commands exposing (toggleTextBlock)
import RichText.Definitions exposing (doc, heading, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline
        , block
        , blockChildren
        , inlineChildren
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
                        (Element.element heading [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


testToggleBlock : Test
testToggleBlock =
    describe "Tests the toggleBlock transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample)
                    (toggleTextBlock
                        (Element.element heading [])
                        (Element.element paragraph [])
                        False
                        example
                    )
        , test "the reverse case works as expected" <|
            \_ ->
                Expect.equal (Ok example)
                    (toggleTextBlock
                        (Element.element heading [])
                        (Element.element paragraph [])
                        False
                        expectedExample
                    )
        ]
