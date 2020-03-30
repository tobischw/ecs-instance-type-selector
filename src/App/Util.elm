module App.Util exposing (viewColumnTitle)

import Html exposing (..)
import Html.Attributes exposing (..)

viewColumnTitle : String -> Html msg
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]


