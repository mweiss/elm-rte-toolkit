module Commands.TestInsertAfterBlockLeaf exposing (..)

import Array
import Expect
import RichText.Commands exposing (insertAfterBlockLeaf)
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
import RichText.Model.Selection exposing (caret)
import RichText.Model.State exposing (State, state)
import Test exposing (Test, describe, test)


emptyParagraph : Block
emptyParagraph =
    block
        (Element.element paragraph [])
        (inlineChildren <| Array.fromList [ plainText "" ])


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "test" ])
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "test" ])
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    , emptyParagraph
                    ]
            )
        )
        (Just <| caret [ 2, 0 ] 0)


testInsertBlock : Test
testInsertBlock =
    describe "Tests the insertBlock transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal
                    (Ok expectedExample)
                    (insertAfterBlockLeaf emptyParagraph example)
        ]
