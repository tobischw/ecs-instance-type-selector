module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Canvas as Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Color
import Html exposing (Html, canvas, div, hr, small, span)
import Html.Attributes exposing (class, style)


view : Grid.Column msg
view =
    Grid.col [ Col.md4, Col.attrs [ class "p-0" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle
                "Results"
            , hr [] []
            , viewConnectionStatus
            , Util.viewColumnTitle
                "Packing"

            -- , viewBrownieGraph
            ]
        ]


viewConnectionStatus : Html msg
viewConnectionStatus =
    div [ class "pb-2" ]
        [ small [ class "text-muted" ] [ Html.text "Initialized · 0 instances fetched · 0 excluded instances" ]
        , Progress.progress [ Progress.value 0, Progress.animated ]
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
        (shapes [ fill (Color.rgb 0.85 0.92 1) ] [ rect ( 0, 0 ) width height ]
            :: renderSlice "Service A" 0 0 100 150
        )


renderSlice : String -> Float -> Float -> Float -> Float -> List Renderable
renderSlice disp x y width height =
    [ shapes [ fill Color.blue ]
        [ rect ( x, y ) width height ]
    ]
