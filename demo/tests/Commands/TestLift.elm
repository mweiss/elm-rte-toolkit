module Commands.TestLift exposing (..)

import Array
import Expect
import RichText.Commands exposing (lift)
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
                                    (inlineChildren <| Array.fromList [ plainText "text" ])
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
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


rangeExample : State
rangeExample =
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
                                    (inlineChildren <| Array.fromList [ plainText "text" ])
                                ]
                        )
                    , block
                        (Element.element blockquote [])
                        (blockChildren <|
                            Array.fromList
                                [ block (Element.element paragraph [])
                                    (inlineChildren <| Array.fromList [ plainText "text" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| range [ 0, 0, 0 ] 0 [ 1, 0, 0 ] 0)


expectedRangeExample : State
expectedRangeExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    , block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
                    ]
            )
        )
        (Just <| range [ 0, 0 ] 0 [ 1, 0 ] 0)


testLift : Test
testLift =
    describe "Tests the lift transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (lift example)
        , test "the example case works with non-zero offset" <|
            \_ ->
                Expect.equal (Ok (expectedExample |> withSelection (Just <| caret [ 0, 0 ] 2)))
                    (lift (example |> withSelection (Just <| caret [ 0, 0, 0 ] 2)))
        , test "range selection lifts all elements in range" <|
            \_ ->
                Expect.equal (Ok expectedRangeExample) (lift rangeExample)
        ]
