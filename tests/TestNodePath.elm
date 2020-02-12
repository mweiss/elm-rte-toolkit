module TestNodePath exposing (..)

import Expect
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, Spec)
import Rte.NodePath exposing (domToEditor, editorToDom)
import Rte.Spec exposing (emptySpec)
import Test exposing (..)


paragraphParams =
    { name = "p", attributes = [], marks = [] }


codeBlockParams =
    { name = "code_block", attributes = [], marks = [] }


crazyBlockParams =
    { name = "crazy_block", attributes = [], marks = [] }


boldMark =
    { name = "bold", attributes = [] }


paragraphNode =
    { parameters = paragraphParams
    , childNodes = InlineLeafList [ TextLeaf { text = "sample", marks = [] } ]
    }


boldParagraphNode =
    { parameters = paragraphParams
    , childNodes =
        InlineLeafList
            [ TextLeaf
                { text = "sample"
                , marks =
                    [ boldMark ]
                }
            ]
    }


crazyBlockNode =
    { parameters = crazyBlockParams
    , childNodes =
        InlineLeafList
            [ TextLeaf
                { text = "sample"
                , marks = []
                }
            ]
    }


codeBlockNode =
    { parameters = codeBlockParams
    , childNodes = InlineLeafList [ TextLeaf { text = "sample", marks = [] } ]
    }


codeBlockToHtmlNode : ElementParameters -> List HtmlNode -> HtmlNode
codeBlockToHtmlNode parameters children =
    ElementNode "pre"
        []
        [ ElementNode "code" [] children
        ]


crazyBlockToHtmlNode : ElementParameters -> List HtmlNode -> HtmlNode
crazyBlockToHtmlNode parameters children =
    ElementNode "div"
        []
        [ ElementNode "img" [] []
        , ElementNode "div" [] [ ElementNode "hr" [] [] ]
        , ElementNode "div" [] children
        ]


boldToHtmlNode : Mark -> List HtmlNode -> HtmlNode
boldToHtmlNode mark children =
    ElementNode "b" [] children


simpleSpec : Spec
simpleSpec =
    { nodes =
        [ { name = "code_block", toHtmlNode = codeBlockToHtmlNode }
        , { name = "crazy_block", toHtmlNode = crazyBlockToHtmlNode }
        ]
    , marks = [ { name = "bold", toHtmlNode = boldToHtmlNode } ]
    }


domToEditorTests : Test
domToEditorTests =
    describe "Tests the transformation function from a dom node path to an editor node path"
        [ test "Test that an empty spec returns the same path" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (domToEditor emptySpec paragraphNode [ 0 ])
        , test "Test the empty path" <|
            \_ ->
                Expect.equal (Just []) (domToEditor emptySpec paragraphNode [])
        , test "Test invalid path" <|
            \_ ->
                Expect.equal Nothing (domToEditor emptySpec paragraphNode [ 1 ])
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


editorToDomTests : Test
editorToDomTests =
    describe "Tests the transformation function from an editor node path to a dom node path"
        [ test "Test that an empty spec returns the same path" <|
            \_ ->
                Expect.equal (Just [ 0 ]) (editorToDom emptySpec paragraphNode [ 0 ])
        , test "Test the empty path" <|
            \_ ->
                Expect.equal (Just []) (editorToDom emptySpec paragraphNode [])
        , test "Test invalid path" <|
            \_ ->
                Expect.equal Nothing (editorToDom emptySpec paragraphNode [ 1 ])
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
