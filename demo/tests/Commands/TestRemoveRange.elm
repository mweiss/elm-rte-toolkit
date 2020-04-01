module Commands.TestRemoveRange exposing (..)

import Array
import Expect
import RichText.Commands exposing (removeRange)
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
                                [ plainText "he"
                                , plainText "rld"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


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
                                [ plainText ""
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
        (Just <| caret [ 0, 0 ] 0)


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
                            Array.fromList [ plainText "", plainText "ple2" ]
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
        (Just <| caret [ 0, 0 ] 0)


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
                            Array.fromList [ plainText "sample1", plainText "ple2" ]
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
        (Just <| caret [ 0, 0 ] 7)


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
                                [ plainText "sample3"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 7)


testState =
    state testDoc Nothing


testRemoveRange : Test
testRemoveRange =
    describe "Tests the removeRange transform"
        [ test "Tests that an error occurs if nothing is selected" <|
            \_ ->
                Expect.equal (Err "Nothing is selected") (removeRange testState)
        , test "Tests that an error occurs if in a collapsed selection" <|
            \_ ->
                Expect.equal (Err "Cannot remove contents of collapsed selection")
                    (removeRange (testState |> withSelection (Just <| caret [ 0, 0 ] 0)))
        , test "Tests that we can successfully remove contents of a single node" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeSingleNode)
                    (removeRange (testState |> withSelection (Just <| singleNodeRange [ 0, 0 ] 0 7)))
        , test "Tests that we can successfully remove contents across multiple nodes" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeMultiNode)
                    (removeRange (testState |> withSelection (Just <| range [ 0, 0 ] 0 [ 0, 2 ] 3)))
        , test "Tests that we can successfully remove contents when inline node is selected" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeInlineNode)
                    (removeRange (testState |> withSelection (Just <| range [ 0, 1 ] 0 [ 0, 2 ] 3)))
        , test "Tests we can successfully remove contents when block leaf is selected" <|
            \_ ->
                Expect.equal (Ok expectedStateRemoveRangeBlockNode)
                    (removeRange (testState |> withSelection (Just <| range [ 1, 1 ] 0 [ 2 ] 0)))
        , test "Tests that removing a range on a small doc works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (removeRange example)
        ]
