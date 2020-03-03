module App.Container exposing (view)

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

view : Grid.Column msg
view =
    Grid.col [ Col.md4, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Detail"
            , viewContainerDetail
            ]
        ]

viewContainerDetail : Html msg
viewContainerDetail =
    Card.config []
        |> Card.header [] [ text "Container 1a" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                   formRow
                    
            ]
        |> Card.view


formRow : List (Html msg)
formRow =
    List.repeat 3 (Form.row[]
        [Form.colLabel [Col.sm3] [text "vCPUS"]
        , Form.col [ Col.sm9 ]
            [input [ type_ "range", class "form-control-range"] [], Form.help [] [text "sample text"]
            ]
        ]
    )