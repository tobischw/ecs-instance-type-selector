module App.Main exposing (..)

import App.Cluster as Cluster
import App.Configuration as Configuration
import App.Instances as Instances
import App.Container as Container
import App.Results as Results
import App.Service as Service
import App.Settings as Settings exposing (update, Msg)
import App.Task as Task
import App.Util as Util
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest(..), application, document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Error(..), decodeString)
import Tuple exposing (first, second)
import Url exposing (..)
import Url.Parser as Url exposing ((</>), Parser)
import Multiselect
import FeatherIcons



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
    , instances : Instances.Model
    , error : Maybe String
    , settings : Settings.Model
    , collapsedSidebar : Bool
    }


-- These Instances model should probably moved in to their own files
-- at some point

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
    | InstancesMsg Instances.Msg
    | ServiceMsg Service.Msg
    | TaskMsg Task.Msg
    | ContainerMsg Container.Msg
    | SettingsMsg Settings.Msg
    | ToggleSidebar


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

        InstancesMsg instancesMsg ->
            let
                msgWithCmd =
                    Instances.update instancesMsg model.instances
            in
            ( { model | instances = first msgWithCmd }, Cmd.map InstancesMsg (second msgWithCmd))

        ConfigurationMsg configurationMsg ->
            ( { model | configuration = Configuration.update configurationMsg model.configuration }, Cmd.none )

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

                settingsState = first msgWithCmd     

                oses = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.excludedSystems))
                instancesExclude = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.excludedInstances))
                regionsInclude = (List.map (\item -> Tuple.first item) (Multiselect.getSelectedValues settingsState.includedRegions))
                instances2 = Instances.update (Instances.SetFilters (Instances.OS) oses) model.instances
                instances3 = Instances.update (Instances.SetFilters (Instances.InstanceType) instancesExclude) (Tuple.first instances2)
                instances4 = Instances.update (Instances.SetFilters (Instances.Region) regionsInclude) (Tuple.first instances3)
                instances5 = Instances.update (Instances.SetPreferredPricing settingsState.preferredPricing) (Tuple.first instances4)
                instances = Tuple.first instances5
            in
            ( { model | settings = first msgWithCmd, instances = instances }, Cmd.map SettingsMsg (second msgWithCmd) )

        ToggleSidebar ->
            ( { model | collapsedSidebar = not model.collapsedSidebar }, Cmd.none )        


urlToDetail : String -> Url -> Detail
urlToDetail basePath url =
    let
        newUrl =
            case basePath of
                "/" ->
                    url

                _ ->
                    { url | path = String.replace basePath "" url.path }
    in
    newUrl
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
        , viewToggleButton model.collapsedSidebar
        ]
    }


viewToggleButton : Bool -> Html Msg
viewToggleButton collapsedSidebar =
    let
        (icon, offset) = 
            case collapsedSidebar of
                True -> (FeatherIcons.arrowRight, "0")
                False -> (FeatherIcons.arrowLeft, "25%")
    in
    
    div [ class "toggle-sidebar-container"
        , style "left" offset ] 
        [ Button.button 
            [ Button.secondary, Button.onClick ToggleSidebar ] 
            [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ] 
        ]


viewContent : Model -> Html Msg
viewContent model =
    let
        columns = [ viewSidebarColumn model, 
                    viewDetailColumn model,
                    viewResultsColumn model
                  ]

        renderedColumns = 
            case model.collapsedSidebar of
                True -> Maybe.withDefault [] (List.tail columns) 
                False -> columns
    in
    Grid.containerFluid [ class "full-height" ]
        [ Grid.row [ Row.attrs [ class "h-100 pt-5" ] ]
            renderedColumns
        ]


viewSidebarColumn : Model -> Grid.Column Msg
viewSidebarColumn model =
    Grid.col [ Col.md3, Col.attrs [ class "p-0 bg-light sidebar"] ]
     [ Html.map ConfigurationMsg (Configuration.view model.configuration) ]


viewDetailColumn : Model -> Grid.Column Msg
viewDetailColumn model =
    Grid.col [ Col.md4, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , hr [] []
            , viewDetail model
            ]
        ]


viewResultsColumn : Model -> Grid.Column Msg
viewResultsColumn model =
    let
        gridSize = 
            if model.collapsedSidebar then
                Col.md8
            else
                Col.md5
    in
    Grid.col [ gridSize, Col.attrs [ class "p-0" ] ]
             [ Maybe.map viewError model.error |> Maybe.withDefault (span [] [])
                , Results.view (Results.Model model.configuration model.instances model.settings)
             ]


viewDetail : Model -> Html Msg
viewDetail model =
    case model.navigation.currentDetail of
        Cluster id ->
            Dict.get id model.configuration.clusters
                |> Maybe.map (\value -> Cluster.view id value)
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
                |> Maybe.map (\value -> Html.map ContainerMsg (Container.view id value model.configuration.daemons))
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


viewError : String -> Html Msg
viewError error =
    div []
        [ Alert.simpleDanger [] [ text error ] ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Navbar.config NavbarMsg
        |> Navbar.attrs [ class "flex-md-nowrap", class "p-0", class "shadow" ]
        |> Navbar.fixTop
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "/", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0", class "p-2" ]
            [ img [ src (model.flags.basePath ++ "ec2.svg"), class "logo" ] [], text "Cluster Prophet" ]
        |> Navbar.customItems
            [ Navbar.textItem [ Spacing.p2Sm, class "muted" ] [ text ("Loaded " ++ (String.fromInt <| List.length model.instances.instances) ++ " total instances") ]
            ]
        |> Navbar.view model.navigation.navbarState



---- HELPERS ----
---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navigation.navbarState NavbarMsg
        , Sub.map SettingsMsg <| Settings.subscriptions model.settings
        , Sub.map InstancesMsg <| Instances.subscriptions model.instances
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
      , instances = Instances.init
      , error = Nothing
      , settings = Settings.init
      , collapsedSidebar = False
      }
    , Cmd.batch [ navbarCmd, Instances.requestInstances ( Instances.defaultRegion, "", Instances.numInstancesBatched ) ]
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
