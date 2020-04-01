module Commands.TestRemoveRangeAndInsert exposing (..)

import Array
import Expect
import RichText.Commands exposing (removeRangeAndInsert)
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
import RichText.Model.Selection exposing (caret, range, singleNodeRange)
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
        (Just <| range [ 0, 0 ] 2 [ 0, 2 ] 2)


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
                                [ plainText "het"
                                , plainText "rld"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 3)


testDoc : Block
testDoc =
    block
        (Element.element doc [])
        (blockChildren <|
            Array.fromList
                [ block
                    (Element.element paragraph [])
                    (inlineChildren <|
                        Array.fromList
                            [ plainText "sample1"
                            , inlineElement (Element.element image []) []
                            , plainText "sample2"
                            ]
                    )
                , block
                    (Element.element paragraph [])
                    (inlineChildren <|
                        Array.fromList
                            [ plainText "sample3"
                            , inlineElement (Element.element image []) []
                            ]
                    )
                , block
                    (Element.element horizontalRule [])
                    Leaf
                ]
        )


expectedStateRemoveRangeSingleNode : State
expectedStateRemoveRangeSingleNode =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "t"
                                , inlineElement (Element.element image []) []
                                , plainText "sample2"
                                ]
                        )
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "sample3"
                                , inlineElement (Element.element image []) []
                                ]
                        )
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 1)


expectedStateRemoveRangeMultiNode : State
expectedStateRemoveRangeMultiNode =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList [ plainText "t", plainText "ple2" ]
                        )
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "sample3"
                                , inlineElement (Element.element image []) []
                                ]
                        )
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 1)


expectedStateRemoveRangeInlineNode : State
expectedStateRemoveRangeInlineNode =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList [ plainText "sample1t", plainText "ple2" ]
                        )
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "sample3"
                                , inlineElement (Element.element image []) []
                                ]
                        )
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 8)


expectedStateRemoveRangeBlockNode : State
expectedStateRemoveRangeBlockNode =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "sample1"
                                , inlineElement (Element.element image []) []
                                , plainText "sample2"
                                ]
                        )
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "sample3t"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 8)


testState =
    state testDoc Nothing


testRemoveRangeAndInsert : Test
testRemoveRangeAndInsert =
    describe "Tests the testRemoveRangeAndInsert transform"
        [ test "Tests that an error occurs if nothing is selected" <|
            \_ ->
                Expect.equal (Err "Nothing is selected") (removeRangeAndInsert "t" testState)
        , test "Tests that an error occurs if in a collapsed selection" <|
            \_ ->
                Expect.equal (Err "Cannot remove contents of collapsed selection")
                    (removeRangeAndInsert "t" (testState |> withSelection (Just <| caret [ 0, 0 ] 0)))
        , test "Tests that we can successfully remove and insert contents of a single node" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeSingleNode)
                    (removeRangeAndInsert "t" (testState |> withSelection (Just <| singleNodeRange [ 0, 0 ] 0 7)))
        , test "Tests that we can successfully remove contents across multiple nodes" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeMultiNode)
                    (removeRangeAndInsert "t" (testState |> withSelection (Just <| range [ 0, 0 ] 0 [ 0, 2 ] 3)))
        , test "Tests that we can successfully remove contents when inline node is selected" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeInlineNode)
                    (removeRangeAndInsert "t" (testState |> withSelection (Just <| range [ 0, 1 ] 0 [ 0, 2 ] 3)))
        , test "Tests we can successfully remove contents when block leaf is selected" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeBlockNode)
                    (removeRangeAndInsert "t" (testState |> withSelection (Just <| range [ 1, 1 ] 0 [ 2 ] 0)))
        , test "Tests that removing a range on a small doc works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (removeRangeAndInsert "t" example)
        ]
