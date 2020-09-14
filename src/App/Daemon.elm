module App.Daemon exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Dict exposing (Dict)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Bootstrap.Breadcrumb exposing (container)
import Bootstrap.Utilities.DomHelper exposing (className)

-- https://learnyouanelm.github.io/pages/07-modules.html

type alias Model = 
    Configuration.Model

type Msg
    = UpdateCPUShare Int String
    | UpdateMemory Int String
    | ConfigurationMsg Configuration.Msg

update : Msg -> Model -> Model
update msg model =
    case msg of
        ConfigurationMsg configMsg ->
            Configuration.update configMsg model
        UpdateCPUShare daemonid val ->
            { model | daemons = Dict.update daemonid (Maybe.map (\daemon -> {daemon | cpuShare = Util.toInt val})) model.daemons}
        UpdateMemory daemonid val ->
            { model | daemons = Dict.update daemonid (Maybe.map (\daemon -> {daemon | memory = Util.toInt val})) model.daemons}

daemonsForContainer : Configuration.Daemons -> Int -> List (Int, Configuration.Daemon)
daemonsForContainer daemons containerid =
    let
        filtered = Dict.filter (\_ daemon -> daemon.containerId == containerid) daemons
    in
        Dict.toList filtered
    

viewDaemon : (Int, Configuration.Daemon) -> Html Msg
viewDaemon (daemonid, daemon) = 
        Card.config [ Card.attrs [Html.Attributes.class "mt-3"]]
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
        div [] [ Html.map ConfigurationMsg (Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (Configuration.AddDaemon containerId) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text ""])
            , div [] data
        ]
        