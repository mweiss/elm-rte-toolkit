module TestAnnotation exposing (..)

import Expect
import RichText.Annotation exposing (isSelectable, selectable)
import RichText.Definitions exposing (doc)
import RichText.Model.Element as Element
import RichText.Model.Node exposing (Block, Children(..), Inline(..), Path, block, plainText)
import RichText.Node
    exposing
        ( Fragment(..)
        , Node(..)
        )
import Set
import Test exposing (Test, describe, test)


testIsSelectable : Test
testIsSelectable =
    describe "Tests that a node is selectable"
        [ test "Test that a text node is selectable" <|
            \_ -> Expect.equal True <| isSelectable (Inline (plainText ""))
        , test "Test that a element node with a selectable mark is selectable" <|
            \_ ->
                Expect.equal True <|
                    isSelectable
                        (Block
                            (block (Element.element doc [] |> Element.withAnnotations (Set.fromList [ selectable ])) Leaf)
                        )
        , test "Test that a element node without a selectable mark is not selectable" <|
            \_ ->
                Expect.equal False <|
                    isSelectable
                        (Block
                            (block (Element.element doc []) Leaf)
                        )
        ]
