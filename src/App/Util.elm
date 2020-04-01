module App.Util exposing (viewColumnTitle, toInt)

import Html exposing (..)
import Html.Attributes exposing (..)

viewColumnTitle : String -> Html msg
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]


toInt : String -> Int
toInt value =
    String.toInt value |> Maybe.withDefault 0
