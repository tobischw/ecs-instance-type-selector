module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Html exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html.Attributes exposing (..)

view : Grid.Column msg
view =
    Grid.col [ Col.md4, Col.attrs [ class "p-0" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [  Util.viewColumnTitle
                "Live Results"
            , div []
                [ Progress.progress [ Progress.value 0, Progress.animated] ]
            , viewBrownieGraph
            ]
        ]

viewBrownieGraph : Html msg
viewBrownieGraph = 
    let
        width = 200
        height = 200
    in
        Canvas.toHtml (width, height)
            [ style "border" "1px solid black" ]
            [ shapes [ fill Color.white ] [ rect (0, 0) width height ]
            , renderSquare
            ]

renderSquare =
  shapes [ fill Color.blue ]
      [ rect (0, 0) 100 100 ]
