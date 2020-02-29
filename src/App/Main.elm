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
            [ Grid.col [ Col.md3, Col.attrs [ class "p-0"] ] [ viewConfiguration model, text "test" ]
            ]
        ]


viewConfiguration : Model -> Html Msg
viewConfiguration model =
    ListGroup.ul
        [ ListGroup.li [ ListGroup.active ] [ text "List item 1" ]
        , ListGroup.li [] [ text "List item 2" ]
        , ListGroup.li [] [ text "List item 3" ]
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
