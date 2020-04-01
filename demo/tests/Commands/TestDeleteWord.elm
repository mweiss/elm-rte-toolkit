module Commands.TestDeleteWord exposing (..)

import Array
import Expect
import RichText.Commands exposing (deleteWord)
import RichText.Definitions exposing (bold, doc, image, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Mark exposing (mark)
import RichText.Model.Node
    exposing
        ( Block
        , Children(..)
        , Inline
        , block
        , blockChildren
        , inlineChildren
        , inlineElement
        , markedText
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
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "this is an ex"
                                , markedText "ample okay" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 11)


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
                                [ plainText "this is an "
                                , markedText " okay" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 11)


expectedRemoveAn : State
expectedRemoveAn =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "this is  ex"
                                , markedText "ample okay" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 8)


expectedRemoveThis : State
expectedRemoveThis =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "thi is an ex"
                                , markedText "ample okay" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 3)


removeInline : State
removeInline =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "s is an ex"
                                , inlineElement (Element.element image []) []
                                , markedText "ample okay" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 2 ] 6)


expectedRemoveInline : State
expectedRemoveInline =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "s is an ex"
                                , inlineElement (Element.element image []) []
                                , markedText "ample " [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 2 ] 6)


testDeleteWord : Test
testDeleteWord =
    describe "Tests the deleteWord transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (deleteWord example)
        , test "Tests that remove a word across multiple text leaves works as expected" <|
            \_ -> Expect.equal (Ok expectedRemoveAn) (deleteWord (example |> withSelection (Just <| caret [ 0, 0 ] 8)))
        , test "Tests that remove a word stops at the beginning of a text block" <|
            \_ -> Expect.equal (Ok expectedRemoveThis) (deleteWord (example |> withSelection (Just <| caret [ 0, 0 ] 3)))
        , test "Tests that remove a word stops at the beginning of an inline node" <|
            \_ -> Expect.equal (Ok expectedRemoveInline) (deleteWord removeInline)
        ]
