module TestSpec exposing (..)

import Array exposing (Array)
import Expect
import RichTextEditor.Model.Mark exposing (mark)
import RichTextEditor.Model.Node
    exposing
        ( Fragment(..)
        , InlineLeaf(..)
        , blockArray
        , blockNode
        , elementParameters
        , emptyTextLeafParameters
        , inlineLeafArray
        , textLeafParametersWithMarks
        , textLeafWithText
        , withText
        )
import RichTextEditor.Spec exposing (htmlToElementArray)
import RichTextEditor.Specs exposing (blockquote, bold, italic, markdown, paragraph)
import Set
import Test exposing (Test, describe, test)


oneParagraph =
    "<p>test</p>"


expectedOneParagraph =
    Array.fromList <|
        [ BlockNodeFragment <|
            Array.fromList
                [ blockNode
                    (elementParameters paragraph [] Set.empty)
                    (inlineLeafArray
                        (Array.fromList
                            [ textLeafWithText "test"
                            ]
                        )
                    )
                ]
        ]


twoParagraphs =
    "<p>test1</p><p>test2</p>"


twoParagraphsBlockFragment =
    Array.fromList
        [ blockNode
            (elementParameters paragraph [] Set.empty)
            (inlineLeafArray
                (Array.fromList
                    [ textLeafWithText "test1"
                    ]
                )
            )
        , blockNode
            (elementParameters paragraph [] Set.empty)
            (inlineLeafArray
                (Array.fromList
                    [ textLeafWithText "test2"
                    ]
                )
            )
        ]


expectedTwoParagraphs =
    Array.fromList <|
        [ BlockNodeFragment <| twoParagraphsBlockFragment ]


justText =
    "test"


justTextInlineFragment =
    Array.fromList
        [ textLeafWithText "test"
        ]


expectedJustText =
    Array.fromList
        [ InlineLeafFragment <| justTextInlineFragment ]


blockquoteAndParagraphs =
    "<blockquote><p>test1</p><p>test2</p></blockquote>"


expectedBlockquoteAndParagraphs =
    Array.fromList
        [ BlockNodeFragment <|
            Array.fromList
                [ blockNode
                    (elementParameters blockquote [] Set.empty)
                    (blockArray twoParagraphsBlockFragment)
                ]
        ]


oneParagraphWithBold =
    "<p><b>test</b></p>"


boldMark =
    mark bold []


italicMark =
    mark italic []


expectedOneParagraphWithBold =
    Array.fromList <|
        [ BlockNodeFragment <|
            Array.fromList
                [ blockNode
                    (elementParameters paragraph [] Set.empty)
                    (inlineLeafArray
                        (Array.fromList
                            [ TextLeaf (emptyTextLeafParameters |> withText "test" |> textLeafParametersWithMarks [ boldMark ])
                            ]
                        )
                    )
                ]
        ]


oneParagraphWithBoldAndItalic =
    "<p><b>tes<i>t</i></b><i>1</i></p>"


expectedOneParagraphWithBoldAndItalic =
    Array.fromList <|
        [ BlockNodeFragment <|
            Array.fromList
                [ blockNode
                    (elementParameters paragraph [] Set.empty)
                    (inlineLeafArray
                        (Array.fromList
                            [ TextLeaf (emptyTextLeafParameters |> withText "tes" |> textLeafParametersWithMarks [ boldMark ])
                            , TextLeaf (emptyTextLeafParameters |> withText "t" |> textLeafParametersWithMarks [ boldMark, italicMark ])
                            , TextLeaf (emptyTextLeafParameters |> withText "1" |> textLeafParametersWithMarks [ italicMark ])
                            ]
                        )
                    )
                ]
        ]


testHtmlToElementArray : Test
testHtmlToElementArray =
    describe "Tests that htmlToElementArray works as expected"
        [ test "Tests that a basic paragraph can be parsed" <|
            \_ -> Expect.equal (Ok expectedOneParagraph) (htmlToElementArray markdown oneParagraph)
        , test "Tests that multiple paragraphs can be parsed" <|
            \_ -> Expect.equal (Ok expectedTwoParagraphs) (htmlToElementArray markdown twoParagraphs)
        , test "Tests that simple text content can be parsed" <|
            \_ -> Expect.equal (Ok expectedJustText) (htmlToElementArray markdown justText)
        , test "Tests that paragraphs wrapped in a code block can be parsed" <|
            \_ -> Expect.equal (Ok expectedBlockquoteAndParagraphs) (htmlToElementArray markdown blockquoteAndParagraphs)
        , test "Tests that a paragraph with bold text works as expected" <|
            \_ -> Expect.equal (Ok expectedOneParagraphWithBold) (htmlToElementArray markdown oneParagraphWithBold)
        , test "Tests that a paragraph with bold and italic text works as expected" <|
            \_ -> Expect.equal (Ok expectedOneParagraphWithBoldAndItalic) (htmlToElementArray markdown oneParagraphWithBoldAndItalic)
        ]
