module Commands.TestBackspaceText exposing (..)

import Array
import Expect
import RichText.Commands exposing (backspaceText)
import RichText.Definitions exposing (bold, doc, horizontalRule, image, paragraph)
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
import RichText.Model.Selection exposing (caret, singleNodeRange)
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
                                [ plainText "text"
                                , markedText "text2" [ mark bold [] ]
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
                                [ plainText "tex"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 3)


expectedExampleOffsetOne : State
expectedExampleOffsetOne =
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
                                , markedText "ext2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


inlineElementState : State
inlineElementState =
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
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 2 ] 0)


testBackspaceText : Test
testBackspaceText =
    describe "Tests the backspaceText transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (backspaceText example)
        , test "Tests that the example case works as expected when the offset is 1" <|
            \_ ->
                Expect.equal (Ok expectedExampleOffsetOne)
                    (backspaceText (example |> withSelection (Just <| caret [ 0, 1 ] 1)))
        , test "it should return an error if the offset is > 1" <|
            \_ ->
                Expect.equal (Err "I use native behavior when doing backspace when the anchor offset could not result in a node change")
                    (backspaceText (example |> withSelection (Just <| caret [ 0, 1 ] 2)))
        , test "it should return an error if it's at the beginning of the document" <|
            \_ ->
                Expect.equal (Err "Cannot backspace if the previous node is a block")
                    (backspaceText (example |> withSelection (Just <| caret [ 0, 0 ] 0)))
        , test "it should return an error if the previous node is an inline element" <|
            \_ ->
                Expect.equal (Err "Cannot backspace if the previous node is an inline leaf")
                    (backspaceText inlineElementState)
        ]
