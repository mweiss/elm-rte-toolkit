module Commands.TestJoinBackward exposing (..)

import Array
import Expect
import RichText.Commands exposing (joinBackward)
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
        (Just <| caret [ 1, 0 ] 0)


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
                                , plainText "text2"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


inlineExample : State
inlineExample =
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
                                [ inlineElement (Element.element image []) []
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


expectedInlineExample : State
expectedInlineExample =
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
                                , inlineElement (Element.element image []) []
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


blockExample : State
blockExample =
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
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


testJoinBackward : Test
testJoinBackward =
    describe "Tests the joinBackward transform"
        [ test "Tests that the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample) (joinBackward example)
        , test "we can join an inline node backward" <|
            \_ -> Expect.equal (Ok expectedInlineExample) (joinBackward inlineExample)
        , test "we cannot join a block leaf with a text block" <|
            \_ ->
                Expect.equal
                    (Err "I can only join backward if the selection is at beginning of a text block")
                    (joinBackward blockExample)
        ]
