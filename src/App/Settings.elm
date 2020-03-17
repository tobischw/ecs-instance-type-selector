module App.Settings exposing (view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (..)
import Html.Attributes exposing (..)


instanceTypes : List String
instanceTypes =
    [ "T2"
    , "M5"
    , "M4"
    , "M3"
    , "C5"
    , "C4"
    , "C3"
    , "X1"
    , "R4"
    , "R3"
    , "P3"
    , "P2"
    , "G3"
    , "F1"
    , "I3"
    ]


view : Html msg
view =
    Card.config []
        |> Card.header [] [ text "Global Settings" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm12 ] [ text "Included Instance Types", Form.help [] [ text "Only checked instance types will be included during the calculation." ] ]
                        ]
                    , div [] (List.map viewInstanceCheckbox (List.sort instanceTypes))
                    , hr [] []
                    ]
            ]
        |> Card.view


viewInstanceCheckbox : String -> Html msg
viewInstanceCheckbox name =
    Form.row []
        [ Form.col [ Col.sm12 ]
            [ Checkbox.checkbox [ Checkbox.id name, Checkbox.checked True ] name ]
        ]
 