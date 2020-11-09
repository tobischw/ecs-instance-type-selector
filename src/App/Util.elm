module App.Util exposing (initRegionsMultiselect, showIf, toInt, formatMegabytes, viewColumnTitle, viewFormCheckbox, viewFormLabel, viewFormRowSlider, randomColorString)

import Random
import App.Constants as Constants
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import Multiselect
import Json.Decode as Json

viewColumnTitle : String -> Html msg
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]


toInt : String -> Int
toInt value =
    String.toInt value |> Maybe.withDefault 0

formatMegabytes : Int -> String
formatMegabytes memoryInMB =
    if memoryInMB < 999 then
        String.fromInt memoryInMB ++ " MiB"

    else if memoryInMB < 999999 then
        String.fromFloat (toFloat memoryInMB / 1000) ++ " GiB"

    else
        String.fromFloat (toFloat memoryInMB / 1000000) ++ " TiB"



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
            [ input [ type_ "range", class "form-control-range", Html.Attributes.min <| String.fromInt min, Html.Attributes.max <| String.fromInt max, Html.Attributes.step <| String.fromInt step, value <| String.fromInt val, onChange msg ] []
            , Form.help [] [ text sublabel ]
            ]
        ]

onChange : (String -> msg) -> Attribute msg
onChange handler = 
  on "change" <| Json.map handler <| Json.at ["target", "value"] Json.string

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


initRegionsMultiselect : Multiselect.Model
initRegionsMultiselect =
    Multiselect.initModel (List.map (\region -> ( region.regionName, region.displayName )) Constants.allRegions) "mselect"


randomColorString : Random.Seed -> String
randomColorString seed0 =
    let
        (red, seed1) = Random.step (Random.int 0 255) seed0 
        (green, seed2) = Random.step (Random.int 0 255) seed1 
        (blue, seed3) = Random.step (Random.int 0 255) seed2 
    in
        "rgb(" ++ String.fromInt red ++ ", " ++ String.fromInt green ++ ", " ++ String.fromInt blue ++ ", 1)"