module Page.SpecFromScratch exposing (..)

import Array
import Html exposing (Html, a, h1, p, text)
import Html.Attributes exposing (href, title)
import Html.Events
import Links exposing (rteToolkit)
import RichTextEditor.Commands as Commands
import RichTextEditor.Editor as Editor exposing (applyCommand)
import RichTextEditor.Model.Attribute
    exposing
        ( Attribute(..)
        , findBoolAttribute
        , replaceOrAddBoolAttribute
        )
import RichTextEditor.Model.Command exposing (Transform, transformCommand)
import RichTextEditor.Model.Decorations exposing (addElementDecoration, emptyDecorations)
import RichTextEditor.Model.Editor
    exposing
        ( Editor
        , InternalEditorMsg
        , Tagger
        , withCommandMap
        )
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Node
    exposing
        ( BlockNode
        , Element
        , Path
        , attributesFromElementParameters
        , blockArray
        , blockNode
        , blockNodeWithElementParameters
        , elementParameters
        , elementParametersFromBlockNode
        , elementParametersWithAttributes
        , inlineLeafArray
        , nameFromElementParameters
        , textLeafWithText
        )
import RichTextEditor.Model.Spec
    exposing
        ( ElementToHtml
        , HtmlToElement
        , NodeDefinition
        , blockNodeContentType
        , emptySpec
        , nodeDefinition
        , textBlockContentType
        , withNodeDefinitions
        )
import RichTextEditor.Model.State as State exposing (State, withRoot)
import RichTextEditor.Node as Node exposing (Node(..), nodeAt)
import RichTextEditor.Spec exposing (defaultHtmlToElement)
import Session exposing (Session)
import Set


type alias Model =
    { session : Session
    , editor : Editor
    }


type Msg
    = InternalMsg InternalEditorMsg
    | ToggleCheckedTodoItem Path Bool
    | GotSession Session


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "New specification"
    , content =
        [ h1 [] [ text "Creating a new specification" ]
        , p []
            [ text """This example shows how you can create a specification from scratch"""
            ]
        , Editor.view InternalMsg decorations model.editor
        , p []
            [ text "You can see the code for this example in the "
            , a
                [ title "git repo"
                , href (rteToolkit ++ "/tree/master/demo/src/Page/SpecFromScratch.elm")
                ]
                [ text "git repo." ]
            ]
        ]
    }


todoInitNode : BlockNode
todoInitNode =
    blockNode
        (elementParameters todoList [] Set.empty)
        (blockArray (Array.fromList [ initialTodoNode "Item 1", initialTodoNode "Item 2" ]))


initialTodoNode : String -> BlockNode
initialTodoNode s =
    blockNode
        (elementParameters item [] Set.empty)
        (inlineLeafArray (Array.fromList [ textLeafWithText s ]))


initialState : State
initialState =
    State.state todoInitNode Nothing


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , editor =
            RichTextEditor.Model.Editor.editor todoSpec initialState
                |> withCommandMap commandBindings
      }
    , Cmd.none
    )


commandBindings =
    Commands.defaultCommandBindings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InternalMsg editorMsg ->
            ( { model | editor = Editor.update editorMsg model.editor }, Cmd.none )

        ToggleCheckedTodoItem path value ->
            ( handleTodoListChecked path value model, Cmd.none )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


todoList : NodeDefinition
todoList =
    nodeDefinition "todo_list" "root" (blockNodeContentType [ "items" ]) todoListToHtml htmlToTodoList


todoListToHtml : ElementToHtml
todoListToHtml _ children =
    ElementNode "ul"
        []
        children


htmlToTodoList : HtmlToElement
htmlToTodoList =
    defaultHtmlToElement "ul"


item : NodeDefinition
item =
    nodeDefinition "todo_item" "items" (textBlockContentType [ "inline" ]) itemToHtml htmlToItem


itemToHtml : ElementToHtml
itemToHtml params children =
    let
        attributes =
            attributesFromElementParameters params

        checked =
            Maybe.withDefault False (findBoolAttribute "checked" attributes)
    in
    ElementNode "li"
        [ ( "class", "todo-list-item" ) ]
        (Array.fromList
            [ ElementNode "div"
                [ ( "contenteditable", "false" ) ]
                (Array.fromList
                    [ ElementNode "div"
                        (if checked then
                            [ ( "class", "checked checkbox" ) ]

                         else
                            [ ( "class", "not-checked checkbox" ) ]
                        )
                        Array.empty
                    ]
                )
            , ElementNode "div" [ ( "class", "todo-list-item-contents" ) ] children
            ]
        )


htmlToItem : HtmlToElement
htmlToItem def node =
    case node of
        ElementNode name _ children ->
            if name == "li" && Array.length children == 2 then
                case Array.get 0 children of
                    Just n ->
                        case n of
                            ElementNode childName attributes _ ->
                                if childName == "input" && List.any (\( k, v ) -> k == "type" && v == "checkbox") attributes then
                                    let
                                        checked =
                                            List.any (\( k, v ) -> k == "checked" && v == "checked") attributes
                                    in
                                    case Array.get 1 children of
                                        Just n2 ->
                                            case n2 of
                                                ElementNode _ _ c ->
                                                    let
                                                        parameters =
                                                            elementParameters def [ BoolAttribute "checked" checked ] Set.empty
                                                    in
                                                    Just ( parameters, c )

                                                _ ->
                                                    Nothing

                                        Nothing ->
                                            Nothing

                                else
                                    Nothing

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


decorations =
    emptyDecorations
        |> addElementDecoration "todo_item" toggleCheckboxDecoration


toggleCheckboxDecoration : Path -> Element -> Path -> List (Html.Attribute Msg)
toggleCheckboxDecoration editorNodePath elementParameters p =
    let
        checked =
            Maybe.withDefault False (findBoolAttribute "checked" (attributesFromElementParameters elementParameters))
    in
    if p == [ 0, 0 ] then
        [ Html.Events.onClick (ToggleCheckedTodoItem editorNodePath (not checked)) ]

    else
        []


handleTodoListChecked : Path -> Bool -> Model -> Model
handleTodoListChecked path value model =
    { model
        | editor =
            Result.withDefault model.editor
                (applyCommand
                    ( "updateCaptionedImageText"
                    , transformCommand <|
                        updateTodoListItem
                            path
                            value
                    )
                    model.editor
                )
    }


updateTodoListItem : Path -> Bool -> Transform
updateTodoListItem path value state =
    let
        r =
            State.root state
    in
    case nodeAt path (State.root state) of
        Nothing ->
            Err "There is no node at the given path"

        Just node ->
            case node of
                Inline _ ->
                    Err "I can only update todo item, but I received an inline node"

                Block bn ->
                    let
                        ep =
                            elementParametersFromBlockNode bn

                        attributes =
                            attributesFromElementParameters ep

                        newAttributes =
                            replaceOrAddBoolAttribute "checked" value attributes

                        newElementParameters =
                            ep |> elementParametersWithAttributes newAttributes

                        newBlockNode =
                            bn |> blockNodeWithElementParameters newElementParameters
                    in
                    if nameFromElementParameters ep /= "todo_item" then
                        Err "I received a node that was not a todo item"

                    else
                        case Node.replace path (Block newBlockNode) r of
                            Err s ->
                                Err s

                            Ok newRoot ->
                                Ok (state |> withRoot newRoot)


todoSpec =
    emptySpec
        |> withNodeDefinitions [ todoList, item ]
