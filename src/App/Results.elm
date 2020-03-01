module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Html exposing (..)
import Html.Attributes exposing (..)


view : Grid.Column msg
view =
    Grid.col [ Col.md5, Col.attrs [ class "p-0" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Settings"
            , div []
                [ div [] [ text "Exclude Instance Types:" ] 
                , div [] [ text "More settings here:" ]]
                , hr [] []
                , Util.viewColumnTitle
                "Live Results"
            , div []
                [ Progress.progress [ ] ]
            ]
        ]
