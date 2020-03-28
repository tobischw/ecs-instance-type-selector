module App.Container exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
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
    = UpdateCPUShare Int Int String
    | UpdateMem Int Int String
    | UpdateIoops Int Int String
    | UpdateBandwidth Int Int String
    

update : Msg -> Model -> Model
update msg model =
    -- Todo: These case expressions need to be cleaned up! They are a bit of a mess right now. Look into Partial Application!
    case msg of
        UpdateCPUShare serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.CPUShare i)})) model.services }
                Nothing ->
                    model
        UpdateMem serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.Memory i)})) model.services }
                Nothing ->
                    model
        UpdateIoops serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.Ioops i)})) model.services }
                Nothing ->
                    model
        UpdateBandwidth serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.Bandwidth i)})) model.services }
                Nothing ->
                    model

viewMemoryLabel : Int -> String
viewMemoryLabel memoryInMB =
    if memoryInMB < 999 then
        String.fromInt memoryInMB ++ " MiB"
    else if memoryInMB < 999999 then
        String.fromFloat (toFloat memoryInMB / 1000) ++ " GiB"
   else
        String.fromFloat (toFloat memoryInMB / 1000000) ++ " TiB"

view : Int -> Int -> Configuration.Container -> Html Msg
view serviceId containerId container =
    Card.config []
        |> Card.header [] [ text container.name]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "CPU Share" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "1", Html.Attributes.max "1024", value <| String.fromInt container.cpuShare, onInput (UpdateCPUShare serviceId containerId) ] []
                            , Form.help [] [ text (String.fromInt container.cpuShare ++ "/1024 shares of CPU time") ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "500", Html.Attributes.max "3904000", value <| String.fromInt container.memory, onInput (UpdateMem serviceId containerId)] []
                            , Form.help [] [ text (viewMemoryLabel container.memory)]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "IOOPS" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "4750", Html.Attributes.max "19000", value <| String.fromInt container.ioops, onInput (UpdateIoops serviceId containerId) ] []
                            , Form.help [] [ text (String.fromInt container.ioops ++ " MiB/sec") ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Bandwidth" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "1", Html.Attributes.max "25", value <| String.fromInt container.bandwidth, onInput (UpdateBandwidth serviceId containerId) ] []
                            , Form.help [] [ text (String.fromInt container.bandwidth ++ " Gb/sec") ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
