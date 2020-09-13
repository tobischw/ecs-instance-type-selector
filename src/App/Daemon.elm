module App.Daemon exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Bootstrap.Breadcrumb exposing (container)

-- https://learnyouanelm.github.io/pages/07-modules.html

type alias Model = 
    Configuration.Model

type Msg
    = UpdateCPUShare Int String
    | UpdateMemory Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCPUShare daemonid val ->
            model
        UpdateMemory daemonid val ->
            model

daemonsForContainer : Configuration.Daemons -> Int -> List (Int, Configuration.Daemon)
daemonsForContainer daemons containerid =
    let
        filtered = Dict.filter (\_ daemon -> daemon.containerId == containerid) daemons
    in
        Dict.toList filtered
    

viewDaemon : (Int, Configuration.Daemon) -> Html Msg
viewDaemon (daemonid, daemon) = 
        Card.config []
        |> Card.header [] [ text daemon.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    -- these Util calls are a bit odd, but do make the code a bit more organized.
                    [ Util.viewFormRowSlider "CPU Share" ((String.fromInt <| daemon.cpuShare) ++ "/1024 CPU Share") daemon.cpuShare 0 1024 10 (UpdateCPUShare daemonid)
                    , hr [] []
                    , Util.viewFormRowSlider "Memory" (Util.formatMegabytes daemon.memory) daemon.memory 50 8000 50 (UpdateMemory daemonid)
                    ]
            ]
        |> Card.view


view : Configuration.Daemons -> Int -> Html Msg
view daemons containerId = 
    let
        kvPairs = daemonsForContainer daemons containerId
        data = List.map viewDaemon kvPairs
    in
        div [] data
        