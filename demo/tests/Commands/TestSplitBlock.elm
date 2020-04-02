module Commands.TestSplitBlock exposing (..)

import Array
import Expect
import RichText.Commands exposing (splitBlock, splitBlockHeaderToNewParagraph, splitTextBlock)
import RichText.Definitions exposing (blockquote, doc, heading, horizontalRule, image, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Node as Node
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
import RichText.Node exposing (findAncestor, findTextBlockNodeAncestor)
import Test exposing (Test, describe, test)


nestedExample : State
nestedExample =
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
                                    (inlineChildren <| Array.fromList [ plainText "" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0, 0 ] 0)


expectedNestedExample : State
expectedNestedExample =
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
                                    (inlineChildren <| Array.fromList [ plainText "" ])
                                ]
                        )
                    , block
                        (Element.element blockquote [])
                        (blockChildren <|
                            Array.fromList
                                [ block (Element.element paragraph [])
                                    (inlineChildren <| Array.fromList [ plainText "" ])
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 1, 0, 0 ] 0)


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
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
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    , block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


blockExample : State
blockExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    , block (Element.element horizontalRule []) Leaf
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


rangeExample : State
rangeExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "test" ])
                    ]
            )
        )
        (Just <| singleNodeRange [ 0, 0 ] 0 3)


rangeExpectedExample : State
rangeExpectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    , block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "t" ])
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


inlineExample : State
inlineExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                , inlineElement (Element.element image []) []
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


inlineExpectedExample : State
inlineExpectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                ]
                        )
                    , block (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ inlineElement (Element.element image []) []
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


headerExample : State
headerExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element heading [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


expectedHeaderExample : State
expectedHeaderExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element heading [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    , block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


findBlockquoteAncestor =
    findAncestor (\b -> Element.name (Node.element b) == "blockquote")


testSplitBlock : Test
testSplitBlock =
    describe "Tests the split block transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal
                    (Ok expectedExample)
                    (splitBlock findTextBlockNodeAncestor example)
        , test "the nested case works as expected" <|
            \_ ->
                Expect.equal
                    (Ok expectedNestedExample)
                    (splitBlock findBlockquoteAncestor nestedExample)
        , test "it should fail if you split a block leaf" <|
            \_ ->
                Expect.equal
                    (Err "I cannot find a proper ancestor to split")
                    (splitBlock findTextBlockNodeAncestor blockExample)
        , test "splitting a range selection works as expected" <|
            \_ ->
                Expect.equal
                    (Ok rangeExpectedExample)
                    (splitBlock findTextBlockNodeAncestor rangeExample)
        , test "splitting a selected inline element works as expected" <|
            \_ ->
                Expect.equal
                    (Ok inlineExpectedExample)
                    (splitBlock findTextBlockNodeAncestor inlineExample)
        ]


testSplitTextBlock : Test
testSplitTextBlock =
    describe "Tests the splitTextBlock transform"
        [ test "the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (splitTextBlock example)
        ]


testSplitBlockHeaderToNewParagraph : Test
testSplitBlockHeaderToNewParagraph =
    describe "Tests the splitBlockHeaderToNewParagraph transform"
        [ test "example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedHeaderExample)
                    (splitBlockHeaderToNewParagraph [ "heading" ] (Element.element paragraph []) headerExample)
        , test "it works like split block in the normal case" <|
            \_ ->
                Expect.equal (Ok expectedExample)
                    (splitBlockHeaderToNewParagraph [ "heading" ] (Element.element paragraph []) example)
        ]
