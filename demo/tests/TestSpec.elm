module TestSpec exposing (..)

import Array exposing (Array)
import Expect
import RichText.Definitions exposing (blockquote, bold, italic, markdown, paragraph)
import RichText.Internal.Spec exposing (htmlToElementArray)
import RichText.Model.Element exposing (element)
import RichText.Model.Mark exposing (mark)
import RichText.Model.Node
    exposing
        ( Inline(..)
        , block
        , blockChildren
        , inlineChildren
        , plainText
        )
import RichText.Model.Text as Text
import RichText.Node exposing (Fragment(..))
import Test exposing (Test, describe, test)


oneParagraph =
    "<p>test</p>"


expectedOneParagraph =
    Array.fromList <|
        [ BlockFragment <|
            Array.fromList
                [ block
                    (element paragraph [])
                    (inlineChildren
                        (Array.fromList
                            [ plainText "test"
                            ]
                        )
                    )
                ]
        ]


twoParagraphs =
    "<p>test1</p><p>test2</p>"


twoParagraphsBlockFragment =
    Array.fromList
        [ block
            (element paragraph [])
            (inlineChildren
                (Array.fromList
                    [ plainText "test1"
                    ]
                )
            )
        , block
            (element paragraph [])
            (inlineChildren
                (Array.fromList
                    [ plainText "test2"
                    ]
                )
            )
        ]


expectedTwoParagraphs =
    Array.fromList <|
        [ BlockFragment <| twoParagraphsBlockFragment ]


justText =
    "test"


justTextInlineFragment =
    Array.fromList
        [ plainText "test"
        ]


expectedJustText =
    Array.fromList
        [ InlineFragment <| justTextInlineFragment ]


blockquoteAndParagraphs =
    "<blockquote><p>test1</p><p>test2</p></blockquote>"


expectedBlockquoteAndParagraphs =
    Array.fromList
        [ BlockFragment <|
            Array.fromList
                [ block
                    (element blockquote [])
                    (blockChildren twoParagraphsBlockFragment)
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
        [ BlockFragment <|
            Array.fromList
                [ block
                    (element paragraph [])
                    (inlineChildren
                        (Array.fromList
                            [ Text (Text.empty |> Text.withText "test" |> Text.withMarks [ boldMark ])
                            ]
                        )
                    )
                ]
        ]


oneParagraphWithBoldAndItalic =
    "<p><b>tes<i>t</i></b><i>1</i></p>"


expectedOneParagraphWithBoldAndItalic =
    Array.fromList <|
        [ BlockFragment <|
            Array.fromList
                [ block
                    (element paragraph [])
                    (inlineChildren
                        (Array.fromList
                            [ Text (Text.empty |> Text.withText "tes" |> Text.withMarks [ boldMark ])
                            , Text (Text.empty |> Text.withText "t" |> Text.withMarks [ boldMark, italicMark ])
                            , Text (Text.empty |> Text.withText "1" |> Text.withMarks [ italicMark ])
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
