module App.Container exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)

type alias Model =
    Configuration.Model


type Msg 
    = UpdateVCPU Int String

-- find a better way to do this!
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateVCPU id value ->
            case String.toInt value of
                Just i ->
                    model
                    --{ model | services = Dict.update id (Maybe.map (\vCPUs -> { vCPUs | vCPUs = i })) model.services }
                Nothing ->
                    model

view : Int -> Configuration.Container -> Html Msg
view serviceId container =
    Card.config []
        |> Card.header [] [ text container.name]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "vCPUs" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt container.vCPUs ] []
                            , Form.help [] [ text "-- Units" ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt container.memory ] []
                            , Form.help [] [ text "-- MiB" ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "IOOPS" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt container.ioops ] []
                            , Form.help [] [ text "-- Mbits/sec" ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
