module Commands.TestInsertText exposing (..)

import Array
import Expect
import RichText.Commands exposing (insertText)
import RichText.Definitions exposing (doc, horizontalRule, image, paragraph)
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
                                [ plainText "teinsertxt" ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 8)


expectedRange : State
expectedRange =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "insertxt" ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 6)


blockSelected : State
blockSelected =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "insertxt" ]
                        )
                    , block
                        (Element.element horizontalRule [])
                        Leaf
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


inlineSelected : State
inlineSelected =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ inlineElement (Element.element image []) [] ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 0)


testInsertText : Test
testInsertText =
    describe "Tests the insertText transform"
        [ test "Tests that the example case works as expected" <|
            \_ -> Expect.equal (Ok expectedExample) (insertText "insert" example)
        , test "it should insert into a range" <|
            \_ ->
                Expect.equal
                    (Ok expectedRange)
                    (insertText "insert"
                        (example
                            |> withSelection (Just <| singleNodeRange [ 0, 0 ] 0 2)
                        )
                    )
        , test "it should fail if a block leaf is selected" <|
            \_ ->
                Expect.equal
                    (Err "I was expecting a text leaf, but instead I found a block node")
                    (insertText "insert" blockSelected)
        , test "it should fail if an inline leaf is selected" <|
            \_ ->
                Expect.equal
                    (Err "I was expecting a text leaf, but instead found an inline element")
                    (insertText "insert" inlineSelected)
        ]
