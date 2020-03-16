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
                div [] [ text "Configuration pending"]
            {-
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm8 ] [ text "EC2 Instance Scaling", Form.help [] [ text "when each EC2 instance reaches 80% or something, then scal eor something ask WILL" ] ]
                        , Form.col [ Col.sm4 ]
                            [ Input.number [ Input.value "1" ] ]
                        ]
                    ]-}
            ]
        |> Card.view
