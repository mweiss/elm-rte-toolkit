module Rte.Model exposing (..)

{-|

    This module contains the types related to creating a rich text editor.

    # Editor Definition
    @docs Editor, EditorState

    # Editor node
    @docs NodePath, TextNodeContents, ElementNodeContents, EditorAttribute, DisplayType, Mark

    # Editor node helpers
    @docs blockNode, blockNodeWithMarks, inlineNode, inlineNodeWithMarks, plainTextNode

    # Selection
    @docs Selection

    # Commands
    @docs CommandMap, CommandBinding, CommandFunc, KeyMap, InputEventMapType

    # Messages
    @docs DecoderFunc, EditorChange, InputEvent, KeyboardEvent, InternalEditorMsg

    # DOM Validation
    @docs DOMNodeContents, domTextNodeType, domElementNodeType, DOMNode

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Html
import Json.Encode as E
import List.Extra
import Set exposing (Set)


type alias Decorations msg =
    { marks : Dict String (List (MarkDecoratorFunction msg))
    , elements : Dict String (List (ElementDecoratorFunction msg))
    }


type alias ElementDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> ElementParameters -> NodePath -> List (Html.Attribute msg)


type alias MarkDecoratorFunction msg =
    DecoderFunc msg -> NodePath -> Mark -> NodePath -> List (Html.Attribute msg)


{-| Represents a rich text editor. The state of the editor, along with render information,
decoder function, and command map.
-}
type alias Editor msg =
    { editorState : EditorState
    , renderCount : Int
    , selectionCount : Int
    , completeRerenderCount : Int
    , isComposing : Bool
    , decoder : DecoderFunc msg
    , bufferedEditorState : Maybe EditorState
    , decorations : Decorations msg
    , commandMap : CommandMap
    , spec : Spec
    }


{-| An EditorState is a tuple of an editor fragment and a selection. EditorState allows you to keep
track of and manipulate the contents of the editor.
-}
type alias EditorState =
    { root : EditorBlockNode
    , selection : Maybe Selection
    }


{-| A mark is a piece of information that can be attached to a node. It can be used to as extra
information when rendering a node (like color, font, and link information).
-}
type alias Mark =
    { name : String, attributes : List EditorAttribute }


type alias Annotation =
    String


type alias ElementParameters =
    { name : String
    , attributes : List EditorAttribute
    , annotations : Set Annotation
    }


elementParameters : String -> List EditorAttribute -> Set Annotation -> ElementParameters
elementParameters name attributes annotations =
    { name = name, attributes = attributes, annotations = annotations }


{-| An editor block node represents a block element in your document. An editor block node can either
have other block nodes as children, have all inline leaf nodes as children, or be a leaf node.
-}
type alias EditorBlockNode =
    { parameters : ElementParameters
    , childNodes : ChildNodes
    }


{-| ChildNodes represents what children an editor block node can have. A block node may have
other block nodes as children, inline leaf nodes as children, or it may be a leaf itself.
-}
type ChildNodes
    = BlockArray (Array EditorBlockNode)
    | InlineLeafArray InlineLeafArrayContents
    | Leaf


type alias InlineLeafArrayContents =
    { array : Array EditorInlineLeaf, tree : Array InlineLeafTree, reverseLookup : Array NodePath }


type alias MarkNodeContents =
    { mark : Mark, children : Array InlineLeafTree }


type InlineLeafTree
    = MarkNode MarkNodeContents
    | LeafNode Int


findMarksFromInlineLeaf : EditorInlineLeaf -> List Mark
findMarksFromInlineLeaf leaf =
    case leaf of
        TextLeaf l ->
            l.marks

        InlineLeaf l ->
            l.marks


inlineLeafArray : Array EditorInlineLeaf -> ChildNodes
inlineLeafArray arr =
    let
        tree =
            marksToMarkNodeList (List.map findMarksFromInlineLeaf (Array.toList arr))
    in
    InlineLeafArray
        { array = arr
        , tree = tree
        , reverseLookup = Array.fromList <| inlineLeafTreeToPaths [] tree
        }


inlineLeafTreeToPaths : NodePath -> Array InlineLeafTree -> List NodePath
inlineLeafTreeToPaths backwardsPath tree =
    List.concatMap
        (\( i, n ) ->
            case n of
                LeafNode _ ->
                    [ List.reverse (i :: backwardsPath) ]

                MarkNode mn ->
                    inlineLeafTreeToPaths (i :: backwardsPath) mn.children
        )
        (List.indexedMap Tuple.pair (Array.toList tree))


marksToMarkNodeList : List (List Mark) -> Array InlineLeafTree
marksToMarkNodeList markLists =
    marksToMarkNodeListRec (List.indexedMap Tuple.pair markLists)


marksToMarkNodeListRec : List ( Int, List Mark ) -> Array InlineLeafTree
marksToMarkNodeListRec indexedMarkLists =
    Array.fromList <|
        List.concatMap
            (\( ( i, ( m, rest ) ), groupRest ) ->
                case m of
                    Nothing ->
                        LeafNode i :: List.map (\( j, _ ) -> LeafNode <| j) groupRest

                    Just mark ->
                        [ MarkNode
                            { mark = mark
                            , children = marksToMarkNodeListRec (( i, rest ) :: List.map (\( j, ( _, r ) ) -> ( j, r )) groupRest)
                            }
                        ]
            )
        <|
            List.Extra.groupWhile
                (\( _, ( m1, _ ) ) ( _, ( m2, _ ) ) -> m1 == m2)
            <|
                List.map (\( i, a ) -> ( i, ( List.head a, List.drop 1 a ) )) indexedMarkLists


type alias InlineLeafContents =
    { marks : List Mark
    , parameters : ElementParameters
    }


{-| An inline leaf node represents an inline element in your document. It can either be an inline leaf node,
like an image or line break, or a text node.
-}
type EditorInlineLeaf
    = InlineLeaf InlineLeafContents
    | TextLeaf TextLeafContents


{-| TextNodeContents represents the attributes that can be in a text node. The core attributes
are marks and text.
-}
type alias TextLeafContents =
    { marks : List Mark
    , annotations : Set Annotation
    , text : String
    }


{-| An editor attribute is a key value pair. It's used to store information on a node or mark,
like color, font type, or image or link locations.
-}
type EditorAttribute
    = StringAttribute String String


{-| A node path is a list of indexes that represent the path from an editor fragment to a node. It's
the main type used to identify where a node is in the editor.
-}
type alias NodePath =
    List Int


{-| A command map holds a map of key combination and input events to actions that should be taken.
You can use this map to create custom commands on key press.
-}
type alias CommandMap =
    { keyMap : KeyMap
    , inputEventTypeMap : InputEventTypeMap
    }


{-| A command binding can either be a key press or an input event. Note that each browser has
varying degrees of Input Level 2 support, so relying just on input events is usually not enough to support
all browsers. On the flip side, virtual keyboards, specifically Android virtual keyboards, can fire
synthetic keyboard events that don't contain the key value, so for key actions on those platforms
you may need rely on input events.
-}
type CommandBinding
    = Key (List String)
    | InputEventType String


{-| A command function takes an editor state and returns a maybe with the new editor state with the command applied.
The idea behind the command function is that a key binding or input event has a different action depending
on the context. For example, on Enter, suppose we want the default behavior to split the block its in. However,
if the selection is in an empty list item, then the behavior is to lift the list item contents out of the list. To achieve
this behavior, we would define a command to lift selected list items that would only return an editor state if
it was applicable, and define a handle insert paragraph command that would return an editor state if applicable. We
then chain the two commands together with the compose function, whose logic is 'do this action first. If the first command succeeds, it uses that
that editor state. Otherwise, apply the next command to the editor state.

commandBindings =
emptyCommandBinding
|> setCommand
[ inputEvent "insertParagraph", key [ Command.enterKey ], key [ Command.returnKey ] ]
(Command.compose liftSelectedListItemCommand handleInsertParagraph)

-}
type alias Command =
    EditorState -> Result String EditorState


{-| A key map is a dictionary of sorted keys to a command function. Keys should be created with the
Command.key function to ensure the list is sorted in the correct order. This is used to determine
what command to execute on the editor's keydown event.
-}
type alias KeyMap =
    Dict (List String) Command


{-| A dictionary of input type event type to command function. This is used to determine what command
to execute on the editor's beforeinput events.
-}
type alias InputEventTypeMap =
    Dict String Command


{-| A selection represents the information received and translated from the selection API. Note that
the anchorNode and focusNode are translations of the node paths relative to the editor.
-}
type alias Selection =
    { anchorOffset : Int
    , anchorNode : NodePath
    , focusOffset : Int
    , focusNode : NodePath
    }


{-| The decoder function is used to translate the messages the editor needs to your overall application.
A typical use case might be:

    type alias Model = { editor : Editor }

    type MyApplicationMsg
        = EditorInternalMsg InternalEditorMsg
        | OtherAppAction1
        | OtherAppAction2

    decoder =
        EditorInternalMsg

    update :
        update : MyApplicationMsg -> Model -> ( Model, Cmd MyApplicationMsg )
        update msg model =
            case msg of
                InternalMsg internalEditorMsg ->
                    ( { model | editor = Editor.internalUpdate internalEditorMsg model.editor }, Cmd.none )
                ...

-}
type alias DecoderFunc msg =
    InternalEditorMsg -> msg


{-| Whenever the elm-editor MutationObserver detects a change, it triggers an editor change event
that the editor has to respond to. Note that it's very important for the editor to respond to every
change event so that the VirtualDOM doesn't try to render when the DOM is not in the state that
it's expecting.
-}
type alias EditorChange =
    { root : E.Value
    , selection : Maybe Selection
    , characterDataMutations : Maybe (List TextChange)
    }


{-| The attributes parsed from an input event.
-}
type alias InputEvent =
    { data : Maybe String, isComposing : Bool, inputType : String }


{-| The attributes parsed from a keyboard event.
-}
type alias KeyboardEvent =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    , isComposing : Bool
    }


{-| The internal events that an editor has to respond to. These events should be mapped via a DecoderFunc.
-}
type InternalEditorMsg
    = SelectionEvent (Maybe Selection) Bool
    | ChangeEvent EditorChange
    | BeforeInputEvent InputEvent
    | KeyDownEvent KeyboardEvent
    | CompositionStart
    | CompositionEnd


{-| HtmlNode is used to determine how to render the editor. We don't use the regular VirtualDOM library
because we can't inspect a node after it has been created. Note that we also don't allow text nodes
in the definition because this type is just for the structural content of the editor.
-}
type HtmlNode
    = ElementNode String (List HtmlAttribute) (Array HtmlNode)
    | TextNode String


type alias HtmlAttribute =
    ( String, String )


type alias MarkDefinition =
    { name : String
    , toHtmlNode : Mark -> Array HtmlNode -> HtmlNode
    }


type alias NodeDefinition =
    { name : String
    , toHtmlNode : ElementParameters -> Array HtmlNode -> HtmlNode
    }


type alias Spec =
    { marks : List MarkDefinition
    , nodes : List NodeDefinition
    }


{-| A minimal representation of DomNode. It's purpose is to validate the contents of the DOM for any
unexpected structural changes that can happen in a contenteditable node before applying changes that may
effect to the virtual DOM.
-}
type alias DomNodeContents =
    { nodeType : Int
    , tagName : Maybe String
    , nodeValue : Maybe String
    , childNodes : Maybe (Array DomNode)
    }


{-| A minimal representation of a DomNode. Since the structure of DomNodeContents is recursive,
we need to define a literal type to avoid infinite recursion.
-}
type DomNode
    = DomNode DomNodeContents


{-|

    A represents a text change at the given path in a editor node or DOM tree.

-}
type alias TextChange =
    ( NodePath, String )


selectionAnnotation : Annotation
selectionAnnotation =
    "__selection__"


selectableAnnotation : Annotation
selectableAnnotation =
    "__selectable__"
