module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Color
import Html exposing (Html, canvas, div)
import Html.Attributes exposing (..)


view : Grid.Column msg
view =
    Grid.col [ Col.md4, Col.attrs [ class "p-0" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle
                "Live Results"
            , div []
                [ Progress.progress [ Progress.value 0, Progress.animated ] ]
            , viewBrownieGraph
            ]
        ]



-- https://chimeces.com/elm-canvas/circle-packing.html maybe this circle packing example could hlep a little bit?


viewBrownieGraph : Html msg
viewBrownieGraph =
    let
        width =
            200

        height =
            200
    in
    Canvas.toHtml ( width, height )
        [ style "border" "none" ]
        [ shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
        , renderSlice "Service A" 0 0 50 30
        ]


renderSlice : String -> Float -> Float -> Float -> Float -> List Renderable
renderSlice disp x y width height =
        [ shapes [ fill Color.blue ]
            [ rect ( x, y ) width height ]
        , text [ font { size = 20, family = "sans-serif" } ] ( 20, 20 ) "Hello world"
        ]
