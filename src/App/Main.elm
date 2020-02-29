module App.Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)



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
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col [ Col.md3, Col.attrs [ class "p-0" ] ] [ viewConfiguration model ]
            , Grid.col [ Col.md3, Col.attrs [ class "p-0" ] ] [ viewDetailedConfiguration model ]
            , Grid.col [ Col.md6, Col.attrs [ class "p-0" ] ] [ viewResults model ]
            ]
        ]


viewConfiguration : Model -> Html Msg
viewConfiguration model =
    div [ class "sidebar px-3" ]
        [ viewColumnTitle "Configuration"
        , ListGroup.ul
            [ ListGroup.li [] [ text "Service A" ]
            , ListGroup.li [ ListGroup.active, ListGroup.attrs [ class "pl-5" ] ] [ text "Task A" ]
            , ListGroup.li [] [ text "Service B" ]
            ]
        , div [] [ text "Would need some buttons here to add task and services "]
        ]


viewDetailedConfiguration : Model -> Html Msg
viewDetailedConfiguration model =
    div [ class "sidebar px-3"] 
    [
        viewColumnTitle "Task A"
        , div [] [ text "Idea: If a user changes configuration on the left, the detailed configuration will show up here."
        , br [] [], text "So for example, the little windows with the sliders would be here, depending on what's selected on the left"]
    ]

viewColumnTitle : String -> Html Msg 
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]

viewResults : Model -> Html Msg
viewResults model =
    div [ class "px-3"]
    [
        viewColumnTitle "Results"
        , text "This is where the calculated changed results would appear in realtime"
    ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    Navbar.config NavbarMsg
        |> Navbar.attrs [ class "flex-md-nowrap", class "p-0", class "shadow" ]
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "#", class "text-center", class "col-sm-3", class "col-md-3", class "mr-0" ]
            [ img [ src "logo.png", class "logo" ] [], text "Cluster Prophet" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#", class "px-3" ] [ text "Resync" ]
            ]
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
