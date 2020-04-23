module App.Main exposing (..)

import App.Cluster as Cluster
import App.Configuration as Configuration
import App.Container as Container
import App.Results as Results
import App.Service as Service
import App.Settings as Settings
import App.Task as Task
import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest(..), application, document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (first, second)
import Url exposing (..)
import Url.Parser as Url exposing ((</>), Parser)



---- MODEL ----


type alias Flags =
    { basePath : String }


type alias Navigation =
    { key : Nav.Key
    , navbarState : Navbar.State
    , currentDetail : Detail
    }


type alias Model =
    { flags : Flags
    , navigation : Navigation
    , configuration : Configuration.Model
    , settings : Settings.Model
    }


type Detail
    = None
    | Cluster Int
    | Service Int
    | Task Int
    | Container Int
    | Settings



---- UPDATE ----


type Msg
    = NavbarMsg Navbar.State
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ConfigurationMsg Configuration.Msg
    | ClusterMsg Cluster.Msg
    | ServiceMsg Service.Msg
    | TaskMsg Task.Msg
    | ContainerMsg Container.Msg
    | SettingsMsg Settings.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ flags, navigation } as model) =
    case msg of
        NavbarMsg state ->
            ( { model
                | navigation = { navigation | navbarState = state }
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.navigation.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | navigation = { navigation | currentDetail = urlToDetail flags.basePath url } }
            , Cmd.none
            )

        ConfigurationMsg configurationMsg ->
            ( { model | configuration = Configuration.update configurationMsg model.configuration }, Cmd.none )

        ClusterMsg clusterMsg ->
            ( { model | configuration = Cluster.update clusterMsg model.configuration }, Cmd.none )

        ServiceMsg serviceMsg ->
            ( { model | configuration = Service.update serviceMsg model.configuration }, Cmd.none )

        TaskMsg taskMsg ->
            ( { model | configuration = Task.update taskMsg model.configuration }, Cmd.none )

        ContainerMsg containerMsg ->
            ( { model | configuration = Container.update containerMsg model.configuration }, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                msgWithCmd =
                    Settings.update settingsMsg model.settings
            in
            ( { model | settings = first msgWithCmd }, Cmd.map SettingsMsg (second msgWithCmd) )


urlToDetail : String -> Url -> Detail
urlToDetail basePath url =
    { url | path = String.replace basePath "" url.path }
        |> Url.parse urlParser
        |> Maybe.withDefault None


urlParser : Parser (Detail -> a) a
urlParser =
    Url.oneOf
        [ Url.map None Url.top
        , Url.map Cluster (Url.s "cluster" </> Url.int)
        , Url.map Service (Url.s "service" </> Url.int)
        , Url.map Container (Url.s "container" </> Url.int)
        , Url.map Task (Url.s "task" </> Url.int)
        , Url.map Settings (Url.s "settings")
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Cluster Prophet"
    , body =
        [ viewNavbar model
        , viewContent model
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    Grid.containerFluid [ class "full-height" ]
        [ Grid.row [ Row.attrs [ class "h-100 pt-5" ] ]
            [ Grid.col [ Col.md3, Col.attrs [ class "p-0 bg-light sidebar" ] ]
                [ Html.map ConfigurationMsg (Configuration.view model.configuration)
                ]
            , viewDetailColumn model
            , Results.view
            ]
        ]


viewDetailColumn : Model -> Grid.Column Msg
viewDetailColumn model =
    Grid.col [ Col.md5, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , hr [] []
            , viewDetail model
            ]
        ]


viewDetail : Model -> Html Msg
viewDetail model =
    case model.navigation.currentDetail of
        Cluster id ->
            Dict.get id model.configuration.clusters
                |> Maybe.map (\value -> Html.map ClusterMsg (Cluster.view id value))
                |> Maybe.withDefault viewNotFoundDetail

        Service id ->
            Dict.get id model.configuration.services
                |> Maybe.map (\value -> Html.map ServiceMsg (Service.view id value))
                |> Maybe.withDefault viewNotFoundDetail

        Task id ->
            Dict.get id model.configuration.services
                |> Maybe.map (\value -> Html.map TaskMsg (Task.view id value (Configuration.getContainers id model.configuration.containers)))
                |> Maybe.withDefault viewNotFoundDetail

        Container id ->
            Dict.get id model.configuration.containers
                |> Maybe.map (\value -> Html.map ContainerMsg (Container.view id value))
                |> Maybe.withDefault viewNotFoundDetail

        Settings ->
            Html.map SettingsMsg (Settings.view model.settings)

        _ ->
            viewNoneDetail


viewNoneDetail : Html Msg
viewNoneDetail =
    span [ class "text-muted align-middle" ]
        [ text "Nothing here. Select a service, task, or container from the left sidebar to start configuring." ]


viewNotFoundDetail : Html Msg
viewNotFoundDetail =
    span [ class "text-muted align-middle" ]
        [ text "Whatever you are looking for does not exist." ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Navbar.config NavbarMsg
        |> Navbar.attrs [ class "flex-md-nowrap", class "p-0", class "shadow" ]
        |> Navbar.fixTop
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "/", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0", class "p-2" ]
            [ img [ src "../ec2.svg", class "logo" ] [], text "Cluster Prophet" ]
        |> Navbar.view model.navigation.navbarState



---- HELPERS ----
---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navigation.navbarState NavbarMsg
        , Sub.map SettingsMsg <| Settings.subscriptions model.settings
        ]



---- PROGRAM ----


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init ({ basePath } as flags) url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { flags = flags
      , navigation =
            { key = key
            , navbarState = navbarState
            , currentDetail = urlToDetail basePath url
            }
      , configuration = Configuration.init
      , settings = Settings.init
      }
    , navbarCmd
    )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
