module List.TestLift exposing (..)

import Array
import Expect
import RichText.Definitions exposing (doc, listItem, orderedList, paragraph)
import RichText.List exposing (defaultListDefinition, lift)
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
                                                        [ plainText "text"
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
        (Just <| caret [ 0, 0, 0, 0 ] 4)


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
        (Just <| caret [ 0, 0 ] 4)


testLift : Test
testLift =
    describe "Tests the lift transform"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (lift defaultListDefinition example)
        ]
