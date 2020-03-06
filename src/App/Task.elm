module App.Task exposing (view)

import App.Container as Container
import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


view : Bool -> Html msg
view withContainers =
    div []
        [ Card.config []
            |> Card.header [] [ text "Task A" ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Total Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range disabled", disabled True ] []
                            , Form.help [] [ text "XXXX Mb" ]
                            ]
                        ]
                ]
            |> Card.view
        , if withContainers then
            div []
                [ hr [] []
                , Util.viewColumnTitle "Containers"
                , div [] (List.repeat 2 (div [ class "pt-2" ] [ Container.view ]))
                ]

          else
            div [] []
        ]
