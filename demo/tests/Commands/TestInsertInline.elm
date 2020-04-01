module Commands.TestInsertInline exposing (..)

import Array
import Expect
import RichText.Commands exposing (insertInline)
import RichText.Definitions exposing (doc, horizontalRule, image, paragraph)
import RichText.Model.Attribute exposing (Attribute(..))
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
import RichText.Model.Selection exposing (caret, singleNodeRange)
import RichText.Model.State exposing (State, state, withSelection)
import Test exposing (Test, describe, test)


img : Inline
img =
    inlineElement (Element.element image []) []


newImg : Inline
newImg =
    inlineElement (Element.element image [ StringAttribute "src" "test" ]) []


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
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


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
                                [ plainText "te"
                                , img
                                , plainText "xt"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


inlineReplace : State
inlineReplace =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ img ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


expectedInlineReplace : State
expectedInlineReplace =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ newImg ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


expectedRangeExample : State
expectedRangeExample =
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
                                , img
                                , plainText "xt"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


hrExample : State
hrExample =
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


testInsertInlineElement : Test
testInsertInlineElement =
    describe "Tests the insertInlineElement transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (insertInline img example)
        , test "it should replace a selected inline element" <|
            \_ -> Expect.equal (Ok expectedInlineReplace) (insertInline newImg inlineReplace)
        , test "it should replace a range selection" <|
            \_ ->
                Expect.equal (Ok expectedRangeExample)
                    (insertInline img
                        (example |> withSelection (Just <| singleNodeRange [ 0, 0 ] 0 2))
                    )
        , test "it should fail if a block is selected" <|
            \_ ->
                Expect.equal (Err "I can not insert an inline element if a block is selected")
                    (insertInline img hrExample)
        ]
