module Commands.TestRemoveSelectedLeafElement exposing (..)

import Array
import Expect
import RichText.Commands exposing (removeSelectedLeafElement)
import RichText.Definitions exposing (doc, horizontalRule, image, paragraph)
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
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "hello"
                                , inlineElement (Element.element image []) []
                                , plainText "world"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


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
                                [ plainText "hello"
                                , plainText "world"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 5)


blockLeaf : State
blockLeaf =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                ]
                        )
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


expectedRemoveBlockLeaf : State
expectedRemoveBlockLeaf =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


testRemoveSelectedLeafElement : Test
testRemoveSelectedLeafElement =
    describe "Tests the removeSelectedLeafElement transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (removeSelectedLeafElement example)
        , test "Tests that removing a block leaf works as expected" <|
            \_ -> Expect.equal (Ok expectedRemoveBlockLeaf) (removeSelectedLeafElement blockLeaf)
        , test "Test that it fails if a leaf is not selected" <|
            \_ ->
                Expect.equal (Err "There's no leaf node at the given selection")
                    (removeSelectedLeafElement
                        (blockLeaf |> withSelection (Just <| caret [ 0 ] 0))
                    )
        , test "Test that it fails if a range is selected" <|
            \_ ->
                Expect.equal (Err "I cannot remove a leaf element if it is not collapsed")
                    (removeSelectedLeafElement
                        (blockLeaf |> withSelection (Just <| range [ 0, 0 ] 0 [ 1 ] 1))
                    )
        ]
