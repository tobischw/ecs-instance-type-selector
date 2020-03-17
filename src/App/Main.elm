module App.Main exposing (..)

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
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (..)
import Url.Parser as Url exposing ((</>), Parser)



---- MODEL ----


type alias Model =
    { navbarState : Navbar.State
    , navKey : Nav.Key
    , currentDetail : Detail
    , services: List (Configuration.Service) -- look into if localStorage works?
    }


type Detail
    = None
    | Service Int
    | Task Int Int
    | Container Int
    | Settings


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState, navKey = key, currentDetail = urlToDetail url, services = [Configuration.Service 0 "Service a" [ Configuration.Task 0 "Task a1" [ Configuration.Container 0 "Container a11"]], Configuration.Service 1 "Tobi's Cool Service" [], Configuration.Service 2 "Will's Garbage Service" [] ] }, navbarCmd )



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
        , Url.map Container (Url.s "container" </> Url.int )
        , Url.map Service (Url.s "service" </> Url.int )
        , Url.map Task (Url.s "task" </> Url.int </> Url.int )
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
            [ Configuration.view model.services
            , viewDetailColumn model
            , Results.view
            ]
        ]


viewDetailColumn : Model -> Grid.Column Msg
viewDetailColumn model =
    Grid.col [ Col.md5, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , viewDetail model
            ]
        ]


{--
updateServiceById : (Photo -> Photo) -> Id -> Feed -> Feed
updateServiceById updatePhoto id feed =
    List.map
        (\photo ->
            if photo.id == id then
                updatePhoto photo

            else
                photo
        )
        feed

-- Or maybe we want to just flatten it?
flattenList : NestedList a -> List a
flattenList nested =
    let
        recur node acc =
            case node of
                Element a ->
                    a :: acc

                Nested list ->
                    List.foldr recur acc list
    in
        recur nested []

--}

findServiceById : Model -> Int -> Maybe Configuration.Service
findServiceById model id =
   List.head (List.filter (\i -> i.id == id) model.services)

findTaskById : Model -> Int -> Maybe Configuration.Task
findTaskById model id =
   List.head (List.filter (\i -> i.id == id) model.services)

viewDetail : Model -> Html Msg
viewDetail model =
    case model.currentDetail of
        Service id ->
            case findServiceById model id of
               Just service ->
                    Service.view service
               Nothing ->
                    viewNotFoundDetail
            
        Task serviceId id ->
            Task.view

        Container id ->
            Container.view

        Settings ->
            Settings.view

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
        |> Navbar.brand [ href "#", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0", class "p-2" ]
            [ img [ src "../logo.png", class "logo" ] [], text "Cluster Prophet" ]
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
