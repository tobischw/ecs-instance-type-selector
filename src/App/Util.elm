module App.Util exposing (viewColumnTitle, icon)

import Html exposing (..)
import Html.Attributes exposing (..)


viewColumnTitle : String -> Html msg
viewColumnTitle title =
    h6 [ class "sidebar-heading text-muted pt-2" ] [ text title ]

icon : String -> Html msg
icon name =
    i [ class ("typcn typcn-" ++ name), class "pr-2", class "icon" ] []