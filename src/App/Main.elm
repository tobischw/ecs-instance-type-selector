module App.Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)

import Html exposing (..)
import Html.Attributes exposing (..)

import App.Util as Util
import App.Configuration as Configuration
import App.Detail as Detail
import App.Results as Results

---- MODEL ----


type alias Model =
    { navbarState : Navbar.State
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState }, navbarCmd )



---- UPDATE ----


type Msg
    = NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "ECS Instance Selector"
    , body =
        [ viewNavbar model
        , viewContent model
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    Grid.containerFluid [ class "full-height" ]
        [ Grid.row [ Row.attrs [ class "h-100 pt-5" ] ]
            [ Configuration.view
            , Detail.view
            , Results.view
            ]
        ]

viewResults : Model -> Html Msg
viewResults model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Results"
        , text "This is where the calculated changed results would appear in realtime"
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Navbar.config NavbarMsg
        |> Navbar.attrs [ class "flex-md-nowrap", class "p-0", class "shadow" ]
        |> Navbar.fixTop
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "#", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0", class "p-2" ]
            [ img [ src "logo.png", class "logo" ] [], text "Cluster Prophet" ]
        |> Navbar.view model.navbarState


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
