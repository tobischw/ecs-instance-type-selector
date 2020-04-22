module App.Results exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color
import Html exposing (Html, canvas, div, hr, small, span, p, strong)
import Html.Attributes as HA exposing (class, style)

type alias Item =
    { id : Int
    , label : String
    , color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }

view : Grid.Column msg
view =
    Grid.col [ Col.md4, Col.attrs [ HA.class "p-0" ] ]
        [ div [ HA.class "px-3", HA.class "pt-1" ]
            [ 
             Util.viewColumnTitle
                "Packing"
            , hr [] []
            , viewChart
            , hr [] []
            , Util.viewColumnTitle
                "Suggested Instance Types"
            , span [HA.class "text-muted"] [ text "Suggestions do not work yet, but it would look like:"]
            ]
        ]


viewConnectionStatus : Html msg
viewConnectionStatus =
    div [ HA.class "pb-2" ]
        [ small [ HA.class "text-muted" ] [ Html.text "Initialized · 0 instances fetched · 0 excluded instances" ]
        , Progress.progress [ Progress.value 0, Progress.animated ]
        ]

viewChart : Html msg
viewChart =
    div []
    [ 
      svg
        [ width "590"
        , height "200"
        , HA.style "background-color" "#eee"
        , HA.style "border" "thin solid #a9a9a9"
        ]
       (List.concatMap viewChartItem [
            { id = 0, color = "#8AD2C0", label = "Service 1", x = 0, y = 0, width = 200, height = 100 },
            { id = 1, color = "#7AB918", label = "Service 2", x = 0, y = 100, width = 85, height = 80 }
        ])
    ]

viewChartItem : Item -> List (Svg msg)
viewChartItem item =
    [ g
        [ transform ("translate(" ++ String.fromFloat item.x ++ ", " ++ String.fromFloat item.y ++ ")")
        ]
        [ rect
            [ x "0"
            , y "0"
            , width (String.fromFloat item.width)
            , height (String.fromFloat item.height)
            , stroke "#a9a9a9"
            , fill item.color
            ]
            []
        , Svg.text_
            [ x (String.fromFloat (item.width / 2.0))
            , y (String.fromFloat (item.height / 2.0))
            , textAnchor "middle"
            , alignmentBaseline "central"
            ]
            [ Svg.text item.label ]
        ]
    ]

