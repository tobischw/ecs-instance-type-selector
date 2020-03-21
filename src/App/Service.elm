module App.Service exposing (view, init, Model, Msg(..), update)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)

init : Model
init =
    { scalingTarget = 50
    }


type alias Model =
    { scalingTarget : Int
    }


type Msg
    = UpdateScalingTarget String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScalingTarget value ->
            case String.toInt value of
               Just i -> { model | scalingTarget = i }
               Nothing -> model


view : Model -> Configuration.Service -> Html Msg
view model service =
    Card.config []
        |> Card.header [] [ text service.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Scaling Target" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt model.scalingTarget, onInput UpdateScalingTarget ] []
                            , Form.help [] [ text "% utilization" ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
