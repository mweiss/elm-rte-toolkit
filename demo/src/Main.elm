module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Basic as Basic
import Page.Examples as Examples
import Page.Home as Home
import Page.Markdown as Markdown
import Page.SpecExtension as SpecExtension
import Page.SpecFromScratch as SpecFromScratch
import Route exposing (Route)
import Session exposing (Session(..))
import Task
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Basic Basic.Model
    | SpecExtension SpecExtension.Model
    | SpecFromScratch SpecFromScratch.Model
    | Markdown Markdown.Model
    | Home Home.Model
    | Examples Examples.Model


type alias Flags =
    {}



-- MODEL


init : f -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session navKey))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect session ->
            Page.view Page.Home Home.notFoundView

        NotFound session ->
            Page.view Page.Home Home.notFoundView

        Home home ->
            viewPage Page.Home GotHomeMsg (Home.view home)

        Basic basic ->
            viewPage Page.Basic GotBasicMsg (Basic.view basic)

        Markdown md ->
            viewPage Page.Markdown GotMarkdownMsg (Markdown.view md)

        SpecExtension se ->
            viewPage Page.SpecExtension GotSpecExtensionMsg (SpecExtension.view se)

        SpecFromScratch sfs ->
            viewPage Page.SpecFromScratch GotSpecFromScratchMsg (SpecFromScratch.view sfs)

        Examples examples ->
            viewPage Page.Examples GotExamplesMsg (Examples.view examples)



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotBasicMsg Basic.Msg
    | GotMarkdownMsg Markdown.Msg
    | GotSpecExtensionMsg SpecExtension.Msg
    | GotSpecFromScratchMsg SpecFromScratch.Msg
    | GotExamplesMsg Examples.Msg
    | GotHomeMsg Home.Msg
    | GotSession Session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Basic ->
            Basic.init session |> updateWith Basic GotBasicMsg model

        Just Route.Markdown ->
            Markdown.init session |> updateWith Markdown GotMarkdownMsg model

        Just Route.SpecExtension ->
            SpecExtension.init session |> updateWith SpecExtension GotSpecExtensionMsg model

        Just Route.SpecFromScratch ->
            SpecFromScratch.init session |> updateWith SpecFromScratch GotSpecFromScratchMsg model

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Examples ->
            Examples.init session
                |> updateWith Examples GotExamplesMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotMarkdownMsg subMsg, Markdown md ) ->
            Markdown.update subMsg md
                |> updateWith Markdown GotMarkdownMsg model

        ( GotBasicMsg subMsg, Basic basic ) ->
            Basic.update subMsg basic
                |> updateWith Basic GotBasicMsg model

        ( GotSpecExtensionMsg subMsg, SpecExtension md ) ->
            SpecExtension.update subMsg md
                |> updateWith SpecExtension GotSpecExtensionMsg model

        ( GotSpecFromScratchMsg subMsg, SpecFromScratch basic ) ->
            SpecFromScratch.update subMsg basic
                |> updateWith SpecFromScratch GotSpecFromScratchMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home m ->
            Sub.map GotHomeMsg (Home.subscriptions m)

        Basic m ->
            Sub.map GotBasicMsg (Basic.subscriptions m)

        Markdown m ->
            Sub.map GotMarkdownMsg (Markdown.subscriptions m)

        Examples m ->
            Sub.map GotExamplesMsg (Examples.subscriptions m)

        SpecExtension m ->
            Sub.map GotSpecExtensionMsg (SpecExtension.subscriptions m)

        SpecFromScratch m ->
            Sub.map GotSpecFromScratchMsg (SpecFromScratch.subscriptions m)



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Basic basic ->
            Basic.toSession basic

        Markdown full ->
            Markdown.toSession full

        SpecExtension e ->
            SpecExtension.toSession e

        SpecFromScratch e ->
            SpecFromScratch.toSession e

        Examples examples ->
            Examples.toSession examples
