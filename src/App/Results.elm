module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


view : Grid.Column msg
view =
    Grid.col [ Col.md5, Col.attrs [ class "p-0" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Settings"
            , div []
                [ span [] [ text "Exclude Instance Types:" ] ]
                , hr [] []
                , Util.viewColumnTitle
                "Results"
            , div []
                [ span [ class "text-muted" ] [ text "No results yet. Create a configuration on the left." ]
                ]
            ]
        ]
