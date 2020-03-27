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
    = UpdateVCPU Int Int String
    | UpdateMem Int Int String
    | UpdateIoops Int Int String
    | UpdateNetwork Int Int String
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateVCPU serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.VCPUS i)})) model.services }
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
        UpdateNetwork serviceId id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Configuration.updateContainers serviceId id model.services (Configuration.Network i)})) model.services }
                Nothing ->
                    model

view : Int -> Int -> Configuration.Container -> Html Msg
view serviceId containerId container =
    Card.config []
        |> Card.header [] [ text container.name]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "vCPUs" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "1", Html.Attributes.max "96", value <| String.fromInt container.vCPUs, onInput (UpdateVCPU serviceId containerId) ] []
                            , Form.help [] [ text (String.fromInt container.vCPUs ++ " vCPUs") ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "0", Html.Attributes.max "3904000", value <| String.fromInt container.memory, onInput (UpdateMem serviceId containerId)] []
                            , Form.help [] [ text (String.fromInt container.memory ++ " MiB")  ]
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
                        [ Form.colLabel [ Col.sm3 ] [ text "Network" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", Html.Attributes.min "50", Html.Attributes.max "152000", value <| String.fromInt container.network, onInput (UpdateNetwork serviceId containerId) ] []
                            , Form.help [] [ text (String.fromInt container.network ++ " MiB/sec") ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
