module App.Visualization exposing (..)

import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h2)
import Html.Attributes exposing (class, style)
import Svg exposing (Svg, g, a, rect, svg, line, text_)
import Svg.Attributes exposing (alignmentBaseline, opacity, fontSize, fill, height, stroke, strokeWidth, strokeDasharray, textAnchor, transform, width, x, y, x1, x2, y1, y2)
import List.Extra exposing (scanl, scanl1)
import App.Util as Util
import App.Configuration exposing (Msg)

widthScale : Float
widthScale = 0.25

heightScale : Float
heightScale = 0.0175

emptyBox : Box 
emptyBox =
    (Box -1 "" "" "" 0 0 0 0 0)

type alias Box =
    { id : Int
    , name : String
    , serviceName : String
    , color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , sortValue : Float
    }

type alias SuggestedVisualization =
    { title: String
    , suggestedWidth: Float
    , suggestedHeight: Float
    , visualization: Visualization
    }



type alias Visualization =
    { 
     width : Float
    , height : Float 
    , boxes : List Box
    }

prepareVisualization : List Box -> Visualization
prepareVisualization boxes =
    let 
        sortedBoxes = List.sortBy .sortValue boxes 
        arrangedBoxes = arrangeDiagonally sortedBoxes 
        maxWidth = visualizationWidth arrangedBoxes 
        maxHeight = visualizationHeight arrangedBoxes 
    in
    (Visualization maxWidth maxHeight arrangedBoxes)


viewVisualization: SuggestedVisualization -> Html msg
viewVisualization suggestion  =
    let
        padding = 60
        suggestedDimensions = (suggestion.suggestedWidth, suggestion.suggestedHeight)
        visualization = suggestion.visualization
    in
    svg
        [ width (String.fromFloat (suggestion.suggestedWidth * widthScale + padding) ++ "px")
        , height (String.fromFloat (suggestion.suggestedHeight * heightScale + padding) ++ "px")
        , style "border" "#a9a9a9"
        ] ( drawSuggestedInstance suggestedDimensions ++
            drawWatermark suggestion.title suggestedDimensions ++
            drawAxisLabels suggestedDimensions ++
            drawFreeSpaceBox visualization suggestedDimensions ++
            (List.concatMap drawBox visualization.boxes) ++
            (List.concatMap (drawAnnotation suggestedDimensions) visualization.boxes)
           )

drawWatermark: String -> (Float, Float) -> List (Svg msg)
drawWatermark text (maxWidth, maxHeight) =
    [ Svg.text_
            [ x (maxWidth / 2 * widthScale |> String.fromFloat)
            , y (maxHeight / 2 * heightScale |> String.fromFloat)
            , fontSize "38px"
            , textAnchor "middle" 
            , alignmentBaseline "central"
            , fill "rgba(0, 0, 0, 0.15)"
            ]
            [ Svg.text text ] 
    ]

drawFreeSpaceBox: Visualization -> (Float, Float) -> List (Svg msg)
drawFreeSpaceBox visualization suggested =
    let
        box = (calculateRemainingBox visualization suggested)

        offset = 25
        x = box.x * widthScale
        y = box.y * heightScale
    in
      drawBox box ++
      drawBoxInfo box x y offset


calculateRemainingBox: Visualization -> (Float, Float) -> Box
calculateRemainingBox visualization (suggestedWidth, suggestedHeight) =
    let
        boxes = visualization.boxes
        lastBox = List.reverse boxes |> List.head |> Maybe.withDefault emptyBox
        x = lastBox.x + lastBox.width
        y = lastBox.y + lastBox.height
        width = suggestedWidth - visualization.width 
        height = suggestedHeight - visualization.height
    in
    (Box -1 "Remaining" "Total" "rgba(200,200,200,0.6)" x y width height 0)


drawSuggestedInstance: (Float, Float) -> List (Svg msg)
drawSuggestedInstance (suggestedWidth, suggestedHeight) =
    [ rect 
           [ width (String.fromFloat (suggestedWidth * widthScale) ++ "px"),
             height (String.fromFloat (suggestedHeight * heightScale) ++ "px"),
             stroke "#a9a9a9", 
             fill "rgba(127, 127, 127, 0.2)"
           ] []
    ]


drawBox: Box -> List (Svg msg)
drawBox box =
    [ g
        [ transform ("translate(" ++ String.fromFloat (box.x * widthScale) ++ ", " ++ String.fromFloat (box.y * heightScale) ++ ")")]
        (
            [ rect
                [ x "0"
                , y "0"
                , width (String.fromFloat (box.width * widthScale) ++ "px")
                , height (String.fromFloat (box.height * heightScale) ++ "px")
                , stroke "#a9a9a9"
                , fill box.color
                , opacity "0.5"
                ] []
            ]
        )
    ] 


drawAxisLabels: (Float, Float) -> List (Svg msg)
drawAxisLabels (suggestedWidth, suggestedHeight) = 
    let
        offset = 10
    in
    [ drawText (suggestedWidth * widthScale / 2, suggestedHeight * heightScale + offset * 2) "CPU" "middle"
    , drawText (suggestedWidth * widthScale + offset, suggestedHeight * heightScale / 2) "Memory" "center"
    ]

drawAnnotation: (Float, Float) -> Box -> List (Svg msg)
drawAnnotation (suggestedWidth, suggestedHeight) box =
    let
        boxWidth = box.width * widthScale
        boxHeight = box.height * heightScale
        yTop = box.y * heightScale
        yBottom = yTop + boxHeight 

        xLeft = box.x * widthScale
        xRight = xLeft + boxWidth
        offsetX = 10
    in
    [ line [ x1 "0"
           , y1 (yBottom |> String.fromFloat)
           , x2 ((suggestedWidth * widthScale) |> String.fromFloat)
           , y2 (yBottom |> String.fromFloat)
           , stroke "#a5a5a5"
           , strokeDasharray "10,10"
           , strokeWidth "1px" ] []
       ] ++ drawBoxInfo box xRight yTop offsetX


drawBoxInfo: Box -> Float -> Float -> Float -> List (Svg msg)
drawBoxInfo box x y offsetX =
    let
        cpuLabel = String.fromFloat box.width
        memLabel = Util.formatMegabytes (round box.height)
        boxLabel = box.name ++ " (" ++ box.serviceName ++ ")"
    in
    [ drawText (x + offsetX, y + 20) boxLabel "left"
    , drawText (x + offsetX, y + 35) ("CPU: " ++ cpuLabel) "left"
    , drawText (x + offsetX, y + 50) ("Mem: " ++ memLabel) "left"
    ]

    
drawText: (Float, Float) -> String -> String -> Svg msg
drawText (xPos, yPos) text anchor =
    Svg.text_
            [ x (xPos |> String.fromFloat)
            , y (yPos |> String.fromFloat)
            , fontSize "12px"
            , textAnchor anchor 
            , alignmentBaseline "central"
            , fill "#4e4e4e"
            ]
            [ Svg.text text ]

visualizationWidth: List Box -> Float
visualizationWidth boxes =
    List.sum (List.map (\box -> box.width) boxes)


visualizationHeight: List Box -> Float
visualizationHeight boxes =
    List.sum (List.map (\box -> box.height) boxes)


arrangeDiagonally: List Box -> List Box
arrangeDiagonally boxes =
    scanl1 (calculateNewPosition) boxes


calculateNewPosition: Box -> Box -> Box
calculateNewPosition current previous =
    let
        newX = previous.x + previous.width 
        newY = previous.y + previous.height 
    in
        { current | x = newX, y = newY }
     