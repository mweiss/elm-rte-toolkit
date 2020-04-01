module Commands.TestDeleteText exposing (..)

import Array
import Expect
import RichText.Commands exposing (deleteText)
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
                                [ plainText "text"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 4)


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
                                , markedText "ext2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


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
                                [ plainText "tex"
                                , markedText "text2" [ mark bold [] ]
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 3)


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
        (Just <| caret [ 0, 0 ] 4)


testDeleteText : Test
testDeleteText =
    describe "Tests the deleteText transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (deleteText example)
        , test "Tests that the example case works as expected when the offset is 1 off from the end" <|
            \_ ->
                Expect.equal (Ok expectedExampleOffsetOne)
                    (deleteText (example |> withSelection (Just <| caret [ 0, 0 ] 3)))
        , test "it should return an error if the offset is > 1 off from the end" <|
            \_ ->
                Expect.equal (Err "I use the default behavior when deleting text when the anchor offset is not at the end of a text node")
                    (deleteText (example |> withSelection (Just <| caret [ 0, 1 ] 2)))
        , test "it should return an error if it's at the end of the document" <|
            \_ ->
                Expect.equal (Err "I cannot do delete because there is no neighboring text node")
                    (deleteText (example |> withSelection (Just <| caret [ 0, 1 ] 5)))
        , test "it should return an error if the previous node is an inline element" <|
            \_ ->
                Expect.equal (Err "Cannot delete if the previous node is an inline leaf")
                    (deleteText inlineElementState)
        ]
