module App.Service exposing (view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)

view : Configuration.Service -> Html msg
view service =
    Card.config []
        |> Card.header [] [ text service.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Scaling Target" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range" ] []
                            , Form.help [] [ text "% utilization" ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
