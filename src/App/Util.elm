module App.Util exposing (viewColumnTitle, toInt, showIf, viewFormRowSlider, viewFormCheckbox)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Html.Events exposing (onInput)

viewColumnTitle : String -> Html msg
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]


toInt : String -> Int
toInt value =
    String.toInt value |> Maybe.withDefault 0


showIf : Bool -> Html msg -> Html msg
showIf condition view =
    if condition then
        view 
    else
        span [] []

viewFormRowSlider : String -> String -> Int -> Int -> Int -> Int -> (String -> msg) -> Html msg
viewFormRowSlider label sublabel val min max step msg =
 Form.row []
    [ Form.colLabel [ Col.sm3 ] [ text label ]
      , Form.col [ Col.sm9 ]
            [ input [ type_ "range", class "form-control-range", Html.Attributes.min <| String.fromInt min, Html.Attributes.max <| String.fromInt max, Html.Attributes.step <| String.fromInt step, value <| String.fromInt val, onInput msg ] []
             , Form.help [] [ text sublabel ]
            ]
    ]

viewFormCheckbox : String -> String -> Bool -> (Bool -> msg) -> Html msg
viewFormCheckbox label sublabel checked msg =
    Form.row []
        [
            Form.col [ Col.offsetSm3, Col.sm10 ]
                [ Checkbox.checkbox [ Checkbox.onCheck msg, Checkbox.checked checked ] label ]
        ]

