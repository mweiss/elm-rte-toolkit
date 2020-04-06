module Page exposing (Page(..), view)

import Browser exposing (Document)
import FontAwesome.Styles
import Html exposing (Html, a, article, button, div, footer, header, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Route exposing (Route)


{-| Determines which navbar link (if any) will be rendered as active.
Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.
-}
type Page
    = Basic
    | Markdown
    | SpecExtension
    | SpecFromScratch
    | Home
    | Examples


{-| Take a page's Html and frames it with a header and footer.
The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.
isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)
-}
view : Page -> { title : String, content : List (Html msg) } -> Document msg
view page { title, content } =
    { title = title
    , body = fontAwesomeStyle :: viewHeader page :: viewContent content :: [ viewFooter ]
    }


fontAwesomeStyle : Html msg
fontAwesomeStyle =
    FontAwesome.Styles.css


viewContent : List (Html msg) -> Html msg
viewContent content =
    article [] content


viewHeader : Page -> Html msg
viewHeader page =
    header []
        [ nav []
            [ a [ class "logo", Route.href Route.Home ]
                [ text "Rich Text Editor Toolkit" ]
            , div [ class "nav-links" ] <|
                navbarLink page Route.Home [ text "Home" ]
                    :: viewMenu page
            ]
        ]


viewMenu : Page -> List (Html msg)
viewMenu page =
    let
        linkTo =
            navbarLink page
    in
    [ linkTo Route.Examples [ text "Examples" ]
    , navbarExternalLink "https://github.com/mweiss/elm-rte-toolkit" [ text "Github" ]
    ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ span [ class "attribution" ]
                [ text "This is a demo for the "
                , a [ href "https://github.com/mweiss/elm-rte-toolkit" ] [ text " Elm Rich Text Editor Toolkit" ]
                , text ". Code & design licensed under BSD-3-Clause License."
                ]
            ]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    a [ classList [ ( "nav-link", True ), ( "active", isActive page route ) ], Route.href route ] linkContent


navbarExternalLink : String -> List (Html msg) -> Html msg
navbarExternalLink href linkContent =
    a [ class "nav-link", Html.Attributes.href href ] linkContent


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Basic, Route.Basic ) ->
            True

        ( Markdown, Route.Markdown ) ->
            True

        _ ->
            False
