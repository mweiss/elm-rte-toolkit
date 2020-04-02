module Commands.TestToggleMark exposing (..)

import Array
import Expect
import RichText.Commands exposing (toggleMark)
import RichText.Definitions exposing (bold, doc, heading, markdown, paragraph)
import RichText.Model.Element as Element
import RichText.Model.Mark exposing (Mark, MarkOrder, ToggleAction(..), mark, markOrderFromSpec)
import RichText.Model.Node exposing (Block, Children(..), Inline, block, blockChildren, inlineChildren, markedText, plainText)
import RichText.Model.Selection exposing (caret)
import RichText.Model.State exposing (State, state)
import Test exposing (Test, describe, test)


boldMark : Mark
boldMark =
    mark bold []


markdownMarkOrder : MarkOrder
markdownMarkOrder =
    markOrderFromSpec markdown


example : State
example =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "text" ])
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
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ markedText "" [ boldMark ], plainText "text" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


testToggleMark : Test
testToggleMark =
    describe "Tests the toggleBlock transform"
        [ test "the example case works as expected" <|
            \_ ->
                Expect.equal (Ok expectedExample)
                    (toggleMark
                        markdownMarkOrder
                        boldMark
                        Add
                        example
                    )
        ]
