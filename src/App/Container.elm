module App.Container exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Util as Util 
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)

type alias Model =
    Configuration.Model

type Msg 
    = UpdateCPUShare Int String
    | UpdateMemory Int String
    | UpdateIoops Int String
    | UpdateBandwidth Int String
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCPUShare id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | cpuShare = Util.toInt value})) model.containers}

        UpdateMemory id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | memory = Util.toInt value})) model.containers}

        UpdateIoops id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | ioops = Util.toInt value})) model.containers}
            
        UpdateBandwidth id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | bandwidth = Util.toInt value})) model.containers}
            
viewMemoryLabel : Int -> String
viewMemoryLabel memoryInMB =
    if memoryInMB < 999 then
        String.fromInt memoryInMB ++ " MiB"
    else if memoryInMB < 999999 then
        String.fromFloat (toFloat memoryInMB / 1000) ++ " GiB"
   else
        String.fromFloat (toFloat memoryInMB / 1000000) ++ " TiB"


viewFormRowSlider : String -> String -> Int -> Int -> Int -> Int -> (String -> Msg) -> Html Msg
viewFormRowSlider label sublabel val min max step msg =
    Form.row []
        [ Form.colLabel [ Col.sm3 ] [ text label ]
        , Form.col [ Col.sm9 ]
             [ input [ type_ "range", class "form-control-range", Html.Attributes.min <| String.fromInt min, Html.Attributes.max <| String.fromInt max, Html.Attributes.step <| String.fromInt step, value <| String.fromInt val, onInput msg ] []
               , Form.help [] [ text sublabel ]
             ]
        ]
                       

view : Int -> Configuration.Container -> Html Msg
view id container =
    Card.config []
        |> Card.header [] [ text container.name]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ viewFormRowSlider "CPU Share" ((String.fromInt <| container.cpuShare) ++ "/1024 CPU Share") container.cpuShare 0 1024 10 (UpdateCPUShare id)
                    , viewFormRowSlider "Memory" (viewMemoryLabel container.memory) container.memory 500 3904000 1000 (UpdateMemory id)
                    , viewFormRowSlider "IOOPs" ((String.fromInt <| container.ioops) ++ " MiB/sec") container.ioops 4750 19000 1000 (UpdateIoops id)
                    , viewFormRowSlider "Bandwidth" ((String.fromInt <| container.bandwidth) ++ " GiB/sec") container.bandwidth 1 25 1 (UpdateBandwidth id)
                    ]
            ]
        |> Card.view
