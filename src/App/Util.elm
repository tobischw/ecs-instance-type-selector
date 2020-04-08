module App.Util exposing (determineContainerMemStep, initRegionsMultiselect, determineMaxContainerMemory, showIf, toInt, viewColumnTitle, viewFormCheckbox, viewFormLabel, viewFormRowSlider)

import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect
import Html.Events exposing (onInput)
import App.Constants as Constants


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


viewFormLabel : String -> String -> String -> Html msg
viewFormLabel label sublabel val =
    Form.row []
        [ Form.colLabel [ Col.sm3 ] [ text label ]
        , Form.col [ Col.sm9, Col.attrs [ class "mt-1" ] ]
            [ text val
            , Form.help [] [ text sublabel ]
            ]
        ]


viewFormCheckbox : String -> String -> Bool -> (Bool -> msg) -> Html msg
viewFormCheckbox label sublabel checked msg =
    Form.row []
        [ Form.col [ Col.offsetSm3, Col.sm10 ]
            [ Checkbox.checkbox [ Checkbox.onCheck msg, Checkbox.checked checked ] label
            , showIf (sublabel /= "") (Form.help [] [ text sublabel ])
            ]
        ]


determineMaxContainerMemory : Bool -> Int
determineMaxContainerMemory useMoreMem =
    if useMoreMem then
        24576000

    else
        32000


determineContainerMemStep : Bool -> Int
determineContainerMemStep extraMemEnabled =
    if extraMemEnabled then
        1000

    else
        250

initRegionsMultiselect: Multiselect.Model
initRegionsMultiselect = 
    Multiselect.initModel (List.map (\region -> (region.regionName, region.displayName))  Constants.allRegions) "mselect"