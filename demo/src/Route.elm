module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- ROUTING


type Route
    = Basic
    | Markdown
    | SpecExtension
    | SpecFromScratch
    | Home
    | Examples


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Examples (s "examples")
        , Parser.map Basic (s "examples" </> s "basic")
        , Parser.map Markdown (s "examples" </> s "markdown")
        , Parser.map SpecExtension (s "examples" </> s "spec-extension")
        , Parser.map SpecFromScratch (s "examples" </> s "spec-from-scratch")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Basic ->
            [ "examples", "basic" ]

        Markdown ->
            [ "examples", "markdown" ]

        SpecExtension ->
            [ "examples", "spec-extension" ]

        SpecFromScratch ->
            [ "examples", "spec-from-scratch" ]

        Examples ->
            [ "examples" ]
