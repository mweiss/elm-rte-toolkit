module Commands.TestDeleteBlock exposing (..)

import Array
import Expect
import RichText.Commands exposing (deleteBlock)
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
import RichText.Specs exposing (bold, doc, image, paragraph)
import Test exposing (Test, describe, test)


testDeleteBlock : Test
testDeleteBlock =
    describe "Tests the backspaceWord transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal "TODO" "NOTYET"
        ]
