module Page.SpecFromScratch exposing (..)

import Array
import Html exposing (Html, a, h1, p, text)
import Html.Attributes exposing (href, title)
import Html.Events
import Links exposing (rteToolkit)
import RichTextEditor.Commands as Commands
import RichTextEditor.Config.Command exposing (Transform, transform)
import RichTextEditor.Config.Decorations exposing (addElementDecoration, emptyDecorations)
import RichTextEditor.Config.NodeDefinition as NodeDefinition
    exposing
        ( ElementToHtml
        , HtmlToElement
        , NodeDefinition
        , defaultHtmlToElement
        , nodeDefinition
        )
import RichTextEditor.Config.Spec
    exposing
        ( emptySpec
        , withNodeDefinitions
        )
import RichTextEditor.Editor as Editor exposing (applyCommand)
import RichTextEditor.Model.Attribute
    exposing
        ( Attribute(..)
        , findBoolAttribute
        , replaceOrAddBoolAttribute
        )
import RichTextEditor.Model.Editor
    exposing
        ( Editor
        , Message
        )
import RichTextEditor.Model.Element as Element exposing (Element, element)
import RichTextEditor.Model.HtmlNode exposing (HtmlNode(..))
import RichTextEditor.Model.Node as Node
    exposing
        ( Block
        , Path
        , block
        , blockChildren
        , inlineChildren
        , plainText
        , withElement
        )
import RichTextEditor.Model.State as State exposing (State, withRoot)
import RichTextEditor.Node as Node exposing (Node(..), nodeAt)
import RichTextEditor.Specs exposing (hardBreak)
import Session exposing (Session)
import Set


type alias Model =
    { session : Session
    , editor : Editor
    }


type Msg
    = InternalMsg Message
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
        , Editor.view InternalMsg decorations commandBindings todoSpec model.editor
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


todoInitNode : Block
todoInitNode =
    block
        (element todoList [] Set.empty)
        (blockChildren (Array.fromList [ initialTodoNode "Item 1", initialTodoNode "Item 2" ]))


initialTodoNode : String -> Block
initialTodoNode s =
    block
        (element item [] Set.empty)
        (inlineChildren (Array.fromList [ plainText s ]))


initialState : State
initialState =
    State.state todoInitNode Nothing


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , editor =
            RichTextEditor.Model.Editor.editor initialState
      }
    , Cmd.none
    )


commandBindings =
    Commands.defaultCommandBindings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InternalMsg editorMsg ->
            ( { model | editor = Editor.update commandBindings todoSpec editorMsg model.editor }, Cmd.none )

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
    nodeDefinition { name = "todo_list", group = "root", contentType = NodeDefinition.blockNode [ "items" ], toHtmlNode = todoListToHtml, fromHtmlNode = htmlToTodoList }


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
    nodeDefinition
        { name = "todo_item"
        , group = "items"
        , contentType = NodeDefinition.textBlock [ "inline" ]
        , toHtmlNode = itemToHtml
        , fromHtmlNode = htmlToItem
        }


itemToHtml : ElementToHtml
itemToHtml params children =
    let
        attributes =
            Element.attributes params

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
                                                            element def [ BoolAttribute "checked" checked ] Set.empty
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
        |> addElementDecoration item toggleCheckboxDecoration


toggleCheckboxDecoration : Path -> Element -> Path -> List (Html.Attribute Msg)
toggleCheckboxDecoration editorNodePath elementParameters p =
    let
        checked =
            Maybe.withDefault False (findBoolAttribute "checked" (Element.attributes elementParameters))
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
                    ( "updateTodoListItem"
                    , transform <|
                        updateTodoListItem
                            path
                            value
                    )
                    todoSpec
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
                            Node.element bn

                        attributes =
                            Element.attributes ep

                        newAttributes =
                            replaceOrAddBoolAttribute "checked" value attributes

                        newElementParameters =
                            ep |> Element.withAttributes newAttributes

                        newBlockNode =
                            bn |> withElement newElementParameters
                    in
                    if Element.name ep /= "todo_item" then
                        Err "I received a node that was not a todo item"

                    else
                        case Node.replace path (Block newBlockNode) r of
                            Err s ->
                                Err s

                            Ok newRoot ->
                                Ok (state |> withRoot newRoot)


todoSpec =
    emptySpec
        |> withNodeDefinitions [ todoList, item, hardBreak ]
