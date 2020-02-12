module TestNodePath exposing (..)

import Expect
import Rte.Model exposing (ChildNodes(..), EditorBlockNode, EditorInlineLeaf(..), ElementParameters, HtmlNode(..), Mark, Spec)
import Rte.NodePath exposing (domToEditor, editorToDom)
import Rte.Spec exposing (childNodesPlaceholder, emptySpec)
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


codeBlockToHtmlNode : ElementParameters -> HtmlNode
codeBlockToHtmlNode parameters =
    ElementNode "pre"
        []
        [ ElementNode "code" [] childNodesPlaceholder
        ]


crazyBlockToHtmlNode : ElementParameters -> HtmlNode
crazyBlockToHtmlNode parameters =
    ElementNode "div"
        []
        [ ElementNode "img" [] []
        , ElementNode "div" [] [ ElementNode "hr" [] [] ]
        , ElementNode "div" [] childNodesPlaceholder
        ]


boldToHtmlNode : Mark -> HtmlNode
boldToHtmlNode mark =
    ElementNode "b" [] childNodesPlaceholder


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
        , test "Test more complicated custom toHtmlNode part ii" <|
            \_ ->
                Expect.equal (Just []) (domToEditor simpleSpec crazyBlockNode [ 2 ])
        , test "Test more complicated custom toHtmlNode part iii" <|
            \_ ->
                Expect.equal (Just []) (domToEditor simpleSpec crazyBlockNode [ 1, 0 ])
        ]
