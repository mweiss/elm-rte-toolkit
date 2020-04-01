module Commands.TestInsertBlock exposing (..)

import Array
import Expect
import RichText.Annotation exposing (addToBlock, removeFromBlock, selectable)
import RichText.Commands exposing (insertBlock)
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
                        (inlineChildren <| Array.fromList [ plainText "test" ])
                    ]
            )
        )
        (Just <| caret [ 0, 0 ] 2)


horizontalBlock : Block
horizontalBlock =
    removeFromBlock selectable <|
        block
            (Element.element horizontalRule [])
            Leaf


selectableHorizontalBlock : Block
selectableHorizontalBlock =
    addToBlock selectable horizontalBlock


expectedExample : State
expectedExample =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "te" ])
                    , horizontalBlock
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "st" ])
                    ]
            )
        )
        (Just <| caret [ 2, 0 ] 0)


expectedSelectable : State
expectedSelectable =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "te" ])
                    , selectableHorizontalBlock
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "st" ])
                    ]
            )
        )
        (Just <| caret [ 1 ] 0)


expectedRange : State
expectedRange =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "" ])
                    , horizontalBlock
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "st" ])
                    ]
            )
        )
        (Just <| caret [ 2, 0 ] 0)


expectedInsertBlockSelected : State
expectedInsertBlockSelected =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "te" ])
                    , selectableHorizontalBlock
                    , selectableHorizontalBlock
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <| Array.fromList [ plainText "st" ])
                    ]
            )
        )
        (Just <| caret [ 2 ] 0)


stateWithInline : State
stateWithInline =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                , inlineElement (Element.element image []) []
                                , plainText "test2"
                                ]
                        )
                    ]
            )
        )
        (Just <| caret [ 0, 1 ] 0)


expectedStateWithInline : State
expectedStateWithInline =
    state
        (block
            (Element.element doc [])
            (blockChildren <|
                Array.fromList
                    [ block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ plainText "test"
                                ]
                        )
                    , horizontalBlock
                    , block
                        (Element.element paragraph [])
                        (inlineChildren <|
                            Array.fromList
                                [ inlineElement (Element.element image []) []
                                , plainText "test2"
                                ]
                        )
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
                    (insertBlock horizontalBlock example)
        , test "a selectable block is selected after insert" <|
            \_ ->
                Expect.equal
                    (Ok expectedSelectable)
                    (insertBlock selectableHorizontalBlock example)
        , test "it should insert into a range selection" <|
            \_ ->
                Expect.equal
                    (Ok expectedRange)
                    (insertBlock horizontalBlock
                        (example |> withSelection (Just <| singleNodeRange [ 0, 0 ] 0 2))
                    )
        , test "it should insert a block if a block is selected" <|
            \_ ->
                Expect.equal
                    (Ok expectedInsertBlockSelected)
                    (insertBlock selectableHorizontalBlock expectedSelectable)
        , -- This is somewhat suspicious... perhaps it should remove the inline element instead
          -- of split it
          test "it should insert if an inline is selected" <|
            \_ ->
                Expect.equal
                    (Ok expectedStateWithInline)
                    (insertBlock horizontalBlock stateWithInline)
        ]
