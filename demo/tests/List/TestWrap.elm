module List.TestWrap exposing (..)

import Array
import Expect
import RichText.Definitions exposing (doc, listItem, orderedList, paragraph)
import RichText.List exposing (ListType(..), defaultListDefinition, wrap)
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
import RichText.Model.Selection exposing (caret, range)
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
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "text2"
                                ]
                        )
                    ]
            )
        )
        (Just <| range [ 0, 0 ] 0 [ 1, 0 ] 0)


expectedExample : State
expectedExample =
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
        (Just <| range [ 0, 0, 0, 0 ] 0 [ 0, 1, 0, 0 ] 0)


testWrap : Test
testWrap =
    describe "Tests the wrap transform"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (wrap defaultListDefinition Ordered example)
        ]
