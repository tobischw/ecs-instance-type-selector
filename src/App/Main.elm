module App.Main exposing (..)

import App.Configuration as Configuration
import App.Service as Service
import App.Container as Container
import App.Results as Results
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest(..), application, document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (..)
import Url.Parser as Url exposing ((</>), Parser)
import App.Util as Util



---- MODEL ----


type alias Model =
    { navbarState : Navbar.State
    , navKey : Nav.Key
    , currentDetail : Detail
    }


type Detail
    = None
    | Service
    | Task
    | Container


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState, navKey = key, currentDetail = urlToDetail url }, navbarCmd )



---- UPDATE ----


type Msg
    = NavbarMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | currentDetail = urlToDetail url }
            , Cmd.none
            )


urlToDetail : Url -> Detail
urlToDetail url =
    url
        |> Url.parse urlParser
        |> Maybe.withDefault None


urlParser : Parser (Detail -> a) a
urlParser =
    Url.oneOf
        [ Url.map None Url.top
        , Url.map Container (Url.s "container")
        , Url.map Service (Url.s "service")
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "ECS Instance Selector"
    , body =
        [ viewNavbar model
        , viewContent model
        ]
    }

viewDetailColumn : Detail -> Grid.Column Msg
viewDetailColumn detail =
    Grid.col [ Col.md4, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , viewDetail detail
            ]
        ]

viewDetail : Detail -> Html Msg
viewDetail detail =
    case detail of
        Container ->
            Container.view

        Service ->
            Service.view

        _ ->
            viewNoneDetail


viewNoneDetail : Html Msg
viewNoneDetail =
     span [] 
            [ text "Nothing here. Select a service, task, or container from the left sidebar to start configuring." ]


viewContent : Model -> Html Msg
viewContent model =
    Grid.containerFluid [ class "full-height" ]
        [ Grid.row [ Row.attrs [ class "h-100 pt-5" ] ]
            [ Configuration.view
            , viewDetailColumn model.currentDetail
            , Results.view
            ]
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
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
