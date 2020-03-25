module TestPath exposing (..)

import Array exposing (Array)
import Expect
import RichText.Internal.Path
    exposing
        ( domToEditor
        , editorToDom
        )
import RichText.Model.Element exposing (element)
import RichText.Model.Mark exposing (mark)
import RichText.Model.Node exposing (Inline(..), block, inlineChildren, plainText)
import RichText.Model.Text as Text exposing (withText)
import SimpleSpec exposing (bold, codeBlock, crazyBlock, paragraph, simpleSpec)
import Test exposing (..)


paragraphParams =
    element paragraph []


codeBlockParams =
    element codeBlock []


crazyBlockParams =
    element crazyBlock []


boldMark =
    mark bold []


paragraphNode =
    block
        paragraphParams
        (inlineChildren <| Array.fromList [ plainText "sample" ])


boldParagraphNode =
    block
        paragraphParams
        (inlineChildren <|
            Array.fromList
                [ Text
                    (Text.empty
                        |> withText "sample"
                        |> Text.withMarks [ boldMark ]
                    )
                ]
        )


crazyBlockNode =
    block
        crazyBlockParams
        (inlineChildren <|
            Array.fromList
                [ plainText "sample" ]
        )


codeBlockNode =
    block
        codeBlockParams
        (inlineChildren <| Array.fromList [ plainText "sample" ])


testDomToEditor : Test
testDomToEditor =
    describe "Tests the transformation function from a dom node path to an editor node path"
        [ test "Test that an empty spec returns the same path" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (domToEditor simpleSpec paragraphNode [ 0 ])
        , test "Test the empty path" <|
            \_ ->
                Expect.equal (Just []) (domToEditor simpleSpec paragraphNode [])
        , test "Test invalid path" <|
            \_ ->
                Expect.equal Nothing (domToEditor simpleSpec paragraphNode [ 1 ])
        , test "Test node spec with custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (domToEditor simpleSpec codeBlockNode [ 0, 0 ])
        , test "Test invalid node with custom toHtmlNode" <|
            \_ ->
                Expect.equal Nothing (domToEditor simpleSpec codeBlockNode [ 1, 0 ])
        , test "Test bold spec with custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (domToEditor simpleSpec boldParagraphNode [ 0, 0 ])
        , test "Test more complicated custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (domToEditor simpleSpec crazyBlockNode [ 2, 0 ])
        , test "Test more complicated custom toHtmlNode but select the parent in the dom tree" <|
            \_ ->
                Expect.equal (Just []) (domToEditor simpleSpec crazyBlockNode [ 2 ])
        , test "Test more complicated custom toHtmlNode but select a sibling node in the dom tree" <|
            \_ ->
                Expect.equal (Just []) (domToEditor simpleSpec crazyBlockNode [ 1, 0 ])
        ]


testEditorToDom : Test
testEditorToDom =
    describe "Tests the transformation function from an editor node path to a dom node path"
        [ test "Test that an empty spec returns the same path" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (editorToDom simpleSpec paragraphNode [ 0 ])
        , test "Test the empty path" <|
            \_ ->
                Expect.equal (Just []) (editorToDom simpleSpec paragraphNode [])
        , test "Test invalid path" <|
            \_ ->
                Expect.equal Nothing (editorToDom simpleSpec paragraphNode [ 1 ])
        , test "Test node spec with custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 0, 0 ]) (editorToDom simpleSpec codeBlockNode [ 0 ])
        , test "Test invalid node with custom toHtmlNode" <|
            \_ ->
                Expect.equal Nothing (editorToDom simpleSpec codeBlockNode [ 1 ])
        , test "Test bold spec with custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 0, 0 ]) (editorToDom simpleSpec boldParagraphNode [ 0 ])
        , test "Test more complicated custom toHtmlNode" <|
            \_ ->
                Expect.equal (Just [ 2, 0 ]) (editorToDom simpleSpec crazyBlockNode [ 0 ])
        ]
