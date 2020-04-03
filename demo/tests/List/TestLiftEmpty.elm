module List.TestLiftEmpty exposing (..)

import Array
import Expect
import RichText.Definitions exposing (doc, listItem, orderedList, paragraph)
import RichText.List exposing (defaultListDefinition, liftEmpty)
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
import RichText.Model.State exposing (State, state, withSelection)
import Test exposing (Test, describe, test)


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element orderedList [])
                        (blockChildren <|
                            Array.fromList <|
                                [ block
                                    (Element.element listItem [])
                                    (blockChildren <|
                                        Array.fromList
                                            [ block
                                                (Element.element paragraph [])
                                                (inlineChildren <|
                                                    Array.fromList
                                                        [ plainText ""
                                                        ]
                                                )
                                            ]
                                    )
                                , block
                                    (Element.element listItem [])
                                    (blockChildren <|
                                        Array.fromList
                                            [ block
                                                (Element.element paragraph [])
                                                (inlineChildren <|
                                                    Array.fromList
                                                        [ plainText "text2"
                                                        ]
                                                )
                                            ]
                                    )
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0, 0, 0 ] 0)


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
                                [ plainText ""
                                ]
                        )
                    , block (Element.element orderedList [])
                        (blockChildren <|
                            Array.fromList <|
                                [ block
                                    (Element.element listItem [])
                                    (blockChildren <|
                                        Array.fromList
                                            [ block
                                                (Element.element paragraph [])
                                                (inlineChildren <|
                                                    Array.fromList
                                                        [ plainText "text2"
                                                        ]
                                                )
                                            ]
                                    )
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


testLift : Test
testLift =
    describe "Tests the liftEmpty transform"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (liftEmpty defaultListDefinition example)
        , test "Tests that a non-empty list node cannot be lifted" <|
            \_ ->
                Expect.equal
                    (Err "I cannot lift a node that is not an empty text block")
                    (liftEmpty defaultListDefinition (example |> withSelection (Just <| caret [ 0, 1, 0, 0 ] 0)))
        ]
