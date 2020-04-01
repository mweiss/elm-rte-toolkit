module Model.TestMark exposing (..)

import Expect
import RichText.Config.MarkDefinition exposing (MarkDefinition, defaultMarkDefinition)
import RichText.Definitions exposing (bold, code, italic, link, markdown)
import RichText.Model.Attribute exposing (Attribute(..))
import RichText.Model.Mark exposing (Mark, MarkOrder, ToggleAction(..), mark, markOrderFromSpec, sort, toggle)
import Test exposing (Test, describe, test)


markdownMarkOrder : MarkOrder
markdownMarkOrder =
    markOrderFromSpec markdown


marksToSort : List Mark
marksToSort =
    [ mark bold [], mark italic [], mark code [], mark link [] ]


expectedMarksAfterSort : List Mark
expectedMarksAfterSort =
    [ mark link [], mark bold [], mark italic [], mark code [] ]


dummy1 : MarkDefinition
dummy1 =
    defaultMarkDefinition "dummy1"


dummy2 : MarkDefinition
dummy2 =
    defaultMarkDefinition "dummy2"


marksOutsideOfSpec : List Mark
marksOutsideOfSpec =
    [ mark bold [], mark dummy2 [], mark italic [], mark dummy1 [], mark code [], mark link [] ]


expectedMarksOutsideOfSpec : List Mark
expectedMarksOutsideOfSpec =
    [ mark dummy1 [], mark dummy2 [], mark link [], mark bold [], mark italic [], mark code [] ]


testSort : Test
testSort =
    describe "Tests that sort works correctly"
        [ test "marks are sorted in the right order" <|
            \_ ->
                Expect.equal expectedMarksAfterSort (sort markdownMarkOrder marksToSort)
        , test "marks not in the spec are sorted by alphabetical order in the beginning of the list" <|
            \_ ->
                Expect.equal expectedMarksOutsideOfSpec (sort markdownMarkOrder marksOutsideOfSpec)
        ]


beforeAddNewMark : List Mark
beforeAddNewMark =
    [ mark bold [] ]


afterAddNewMark : List Mark
afterAddNewMark =
    [ mark bold [], mark italic [] ]


beforeAddExistingMark : List Mark
beforeAddExistingMark =
    [ mark link [ StringAttribute "href" "google.com" ] ]


afterAddExistingMark : List Mark
afterAddExistingMark =
    [ mark link [ StringAttribute "href" "yahoo.com" ] ]


testToggle : Test
testToggle =
    describe "Tests that toggle works correctly"
        [ test "adding a new mark works as expected" <|
            \_ ->
                Expect.equal
                    afterAddNewMark
                    (toggle Add markdownMarkOrder (mark italic []) beforeAddNewMark)
        , test "adding a mark with the same name replaces the current mark" <|
            \_ ->
                Expect.equal
                    afterAddExistingMark
                    (toggle Add markdownMarkOrder (mark link [ StringAttribute "href" "yahoo.com" ]) beforeAddExistingMark)
        , test "removing a mark works as expected" <|
            \_ ->
                Expect.equal
                    [ mark bold [], mark italic [] ]
                    (toggle Remove markdownMarkOrder (mark code []) [ mark code [], mark bold [], mark italic [] ])
        , test "flipping a mark that's not in the list should add it" <|
            \_ ->
                Expect.equal
                    [ mark bold [], mark italic [], mark code [] ]
                    (toggle Flip markdownMarkOrder (mark code []) [ mark bold [], mark italic [] ])
        , test "flipping a mark that's in the list should remove it" <|
            \_ ->
                Expect.equal
                    [ mark bold [], mark italic [] ]
                    (toggle Flip markdownMarkOrder (mark code []) [ mark bold [], mark italic [], mark code [] ])
        ]
