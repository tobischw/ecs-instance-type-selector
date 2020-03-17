module App.Task exposing (view)

import App.Container as Container
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div []
        [ Card.config []
            |> Card.header [] [ text "Task A" ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Total Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range disabled", disabled True ] []
                            , Form.help [] [ text "-- MiB · Memory limit of all containers in this task for scaling purposes" ]
                            ]
                        ]
                ]
            |> Card.view
        ]
