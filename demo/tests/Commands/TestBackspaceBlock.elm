module Commands.TestBackspaceBlock exposing (..)

import Array
import Expect
import RichText.Commands exposing (backspaceBlock)
import RichText.Definitions exposing (doc, horizontalRule, paragraph)
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
                        (inlineChildren <| Array.fromList [ plainText "p1" ])
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "p2" ])
                    ]
            )
        )
        (Just <| caret [ 2, 0 ] 0)


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "p1" ])
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "p2" ])
                    ]
            )
        )
        (Just <| caret [ 1, 0 ] 0)


testBackspaceBlock : Test
testBackspaceBlock =
    describe "Tests the backspaceBlock transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (backspaceBlock example)
        , test "it should give an error if the selection is not at the beginning of a text block" <|
            \_ ->
                Expect.equal (Err "Cannot backspace a block element if we're not at the beginning of a text block")
                    (backspaceBlock (example |> withSelection (Just <| caret [ 1, 0 ] 1)))
        , test "it should give an error if the selection is a range" <|
            \_ ->
                Expect.equal (Err "Cannot backspace a block element if we're not at the beginning of a text block")
                    (backspaceBlock (example |> withSelection (Just <| singleNodeRange [ 1, 0 ] 0 1)))
        ]
