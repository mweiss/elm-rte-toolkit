module TestNodePath exposing (..)

import Array exposing (Array)
import Expect
import RichTextEditor.Model.Mark exposing (mark)
import RichTextEditor.Model.Node exposing (InlineLeaf(..), blockNode, elementParameters, emptyTextLeafParameters, inlineLeafArray, textLeafParametersWithMarks, textLeafWithText, withText)
import RichTextEditor.Model.Spec exposing (emptySpec)
import RichTextEditor.NodePath
    exposing
        ( commonAncestor
        , decrement
        , domToEditor
        , editorToDom
        , increment
        , parent
        , toString
        )
import Set
import SimpleSpec exposing (simpleSpec)
import Test exposing (..)


paragraphParams =
    elementParameters "p" [] Set.empty


codeBlockParams =
    elementParameters "code_block" [] Set.empty


crazyBlockParams =
    elementParameters "crazy_block" [] Set.empty


boldMark =
    mark "bold" []


paragraphNode =
    blockNode
        paragraphParams
        (inlineLeafArray <| Array.fromList [ textLeafWithText "sample" ])


boldParagraphNode =
    blockNode
        paragraphParams
        (inlineLeafArray <|
            Array.fromList
                [ TextLeaf
                    (emptyTextLeafParameters
                        |> withText "sample"
                        |> textLeafParametersWithMarks [ boldMark ]
                    )
                ]
        )


crazyBlockNode =
    blockNode
        crazyBlockParams
        (inlineLeafArray <|
            Array.fromList
                [ textLeafWithText "sample" ]
        )


codeBlockNode =
    blockNode
        codeBlockParams
        (inlineLeafArray <| Array.fromList [ textLeafWithText "sample" ])


testDomToEditor : Test
testDomToEditor =
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


testEditorToDom : Test
testEditorToDom =
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


testCommonAncestor : Test
testCommonAncestor =
    describe "Tests that commonAncestor works as expected"
        [ test "Test that the we can find the common ancestor of root and another path" <|
            \_ -> Expect.equal [] (commonAncestor [] [ 0, 1 ])
        , test "Test that the we can find the common ancestor of parent and another path" <|
            \_ -> Expect.equal [ 0 ] (commonAncestor [ 0 ] [ 0, 1 ])
        , test "Test that the we can find the common ancestor of two siblings" <|
            \_ -> Expect.equal [ 0 ] (commonAncestor [ 0, 2 ] [ 0, 1 ])
        , test "Test that the we can find the common of two long paths" <|
            \_ -> Expect.equal [ 0, 1 ] (commonAncestor [ 0, 1, 2, 3, 4 ] [ 0, 1, 3, 2, 4 ])
        ]


testDecrement : Test
testDecrement =
    describe "Tests that decrement works as expected"
        [ test "Decrementing the root path gives the root path" <|
            \_ -> Expect.equal [] (decrement [])
        , test "Decrementing a path should work as expected" <|
            \_ -> Expect.equal [ 0, 1 ] (decrement [ 0, 2 ])
        , test "Decrementing can go negative" <|
            \_ -> Expect.equal [ 0, -1 ] (decrement [ 0, 0 ])
        ]


testIncrement : Test
testIncrement =
    describe "Tests that increment works as expected"
        [ test "Incrementing the root path gives the root path" <|
            \_ -> Expect.equal [] (increment [])
        , test "Incrementing a path should work as expected" <|
            \_ -> Expect.equal [ 0, 2 ] (increment [ 0, 1 ])
        ]


testParent : Test
testParent =
    describe "Tests that parent works as expected"
        [ test "The parent of the root path is the root path" <|
            \_ -> Expect.equal [] (parent [])
        , test "The parent should be the same list with the last element removed" <|
            \_ -> Expect.equal [ 1, 2, 3 ] (parent [ 1, 2, 3, 4 ])
        ]


testToString : Test
testToString =
    describe "Tests that toString works as expected"
        [ test "The root path toString is the empty string" <|
            \_ -> Expect.equal "" (toString [])
        , test "The path toString should work for a path of length 1" <|
            \_ -> Expect.equal "1" (toString [ 1 ])
        , test "The path toString should work for a path of length > 1" <|
            \_ -> Expect.equal "1:3:0" (toString [ 1, 3, 0 ])
        ]
