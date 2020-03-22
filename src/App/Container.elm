module App.Container exposing (view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    Card.config []
        |> Card.header [] [ text "Container 1a - TODO: containers not hooked up to model yet!" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "vCPUs" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range" ] []
                            , Form.help [] [ text "-- Units" ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range" ] []
                            , Form.help [] [ text "-- MiB" ]
                            ]
                        ]
                        ,
                        Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "IOOPS" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range" ] []
                            , Form.help [] [ text "-- Mbits/sec" ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
