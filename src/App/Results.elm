module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances, isSuitableInstance)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h2)
import Html.Attributes exposing (class, style)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Svg exposing (Svg, g, a, rect, svg, line, text_)
import Svg.Attributes exposing (alignmentBaseline, xlinkHref, fontSize, fill, height, stroke, strokeWidth, strokeDasharray, textAnchor, transform, width, x, y, x1, x2, y1, y2)
import App.Configuration exposing (Daemons)
import List.Extra exposing (scanl, scanl1)


widthScale : Float
widthScale = 0.25

heightScale : Float
heightScale = 0.0175


type alias Model = 
    {
     configuration : Configuration.Model
    , instances: Instances.Model
    }


type alias ContainerData =
    { name : String
    , color: String
    }


type alias Box =
    { id : Int
    , name : String
    , color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Visualization =
    { width : Float
    , height : Float 
    , boxes : List Box
    }

view : Model -> Html msg
view model =
    div [ class "pt-1", class "px-3" ]
        [ Util.viewColumnTitle
            "Results"
        , hr [] []
        , viewResultsForService model
        ]


viewResultsForService : Model -> Html msg
viewResultsForService model =
    let
        boxes = List.map convertToBox (Dict.toList model.configuration.containers)
        visualization = prepareVisualization boxes 
        share = round <| visualization.width / 1024
        memory = round <| visualization.height
        showSuggestions = (Dict.isEmpty model.configuration.containers == False)

        (topSuggestion, remainingSuggestions) = 
            if showSuggestions then
                Instances.findOptimalSuggestions model.instances.filters model.instances.instances share memory 5
            else
               (Instances.defaultInstance, []) 

        topWidth = (toFloat topSuggestion.vCPU * 1024)
        topHeight = (toFloat topSuggestion.memory)
    in
    div []
        [ 
          if showSuggestions then 
          div [] [  
          viewVisualization visualization (topWidth, topHeight)
        , hr [] []
        , text ("Ideal CPU share: " ++ String.fromInt share)
        , br [] []
        , text ("Ideal memory: " ++ Util.formatMegabytes memory) 
        , hr [] []
        , h2 [] [ text "Total: $0/mo"]
        , strong [] [ text "Top Suggestion:"]
        , viewInstanceListing topSuggestion
        , hr [] []
        , strong [] [ text "Other Suggestions:"]
        , viewSuggestions remainingSuggestions 
        , hr [] [] ]
        else
            span [] [ text "No results or suggestions available yet."]
        ]


viewSuggestions : Instances -> Html msg 
viewSuggestions instances = 
    div []
        (List.map viewInstanceListing instances)


viewInstanceListing : Instance -> Html msg
viewInstanceListing instance =
    div [ style "margin-top" "10px"] [
        Card.config []
        |> Card.block []
            [ Block.text [] [ strong [] [ text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> Util.formatMegabytes) ++ " (" ++ instance.operatingSystem ++")") ] ] 
            , Block.custom <| div [] [ viewPriceList instance.onDemandPrices ]
            , Block.custom <| div [] [ viewPriceList instance.reservedPrices ]
            ]
        |> Card.view
    ]


viewPriceList : Instances.PriceTerm -> Html msg 
viewPriceList priceTerm =
    case priceTerm of
        Instances.OnDemand prices ->
            if List.length prices > 0 then
                div [] [ text "OnDemand:", ul [] (List.map viewPrice prices) ]
            else
                span [] []

        Instances.Reserved prices ->
            if List.length prices > 0 then
                div [] [ text "Reserved:", ul [] (List.map viewPrice prices) ]
            else
                span [] []


viewPrice :  Instances.Price -> Html msg
viewPrice price =
    case price of
        Instances.Upfront value ->
            li [] [ text <| "$" ++ String.fromFloat value ++ " upfront" ]

        Instances.Hourly value ->
            li [] [ text <| "$" ++ String.fromFloat value ++ "/hr" ]


convertToBox : (Int, Configuration.Container) -> Box
convertToBox (id, container) =
    (Box id container.name container.color 0 0 (toFloat container.cpuShare) (toFloat container.memory))


prepareVisualization : List Box -> Visualization
prepareVisualization boxes =
    let 
        sortedBoxes = List.sortBy calculateBoxArea boxes 
        arrangedBoxes = arrangeDiagonally sortedBoxes 
        maxWidth = visualizationWidth arrangedBoxes 
        maxHeight = visualizationHeight arrangedBoxes 
    in
    (Visualization maxWidth maxHeight arrangedBoxes)


viewVisualization: Visualization -> (Float, Float) -> Html msg
viewVisualization visualization (suggestedWidth, suggestedHeight) =
    let
        padding = 60
    in
    svg
        [ width (String.fromFloat (suggestedWidth * widthScale + padding) ++ "px")
        , height (String.fromFloat (suggestedHeight * heightScale + padding) ++ "px")
        , style "border" "#a9a9a9"

        -- Clean this up, this doesn't look too good
         ] ( drawSuggestedBox (suggestedWidth, suggestedHeight) ++
             drawAxisLabels (suggestedWidth, suggestedHeight) ++
            (List.concatMap drawBox visualization.boxes) ++
            (List.concatMap (drawAnnotation (suggestedWidth, suggestedHeight)) visualization.boxes)
           )

calculateRemainingBox: Visualization -> Box
calculateRemainingBox visualization =
    let
        x = 5
        y = 5
        width = 20
        height = 20
    in
    (Box -1 "" "" x y width height)

drawSuggestedBox: (Float, Float) -> List (Svg msg)
drawSuggestedBox (suggestedWidth, suggestedHeight) =
    [ rect 
           [ width (String.fromFloat (suggestedWidth * widthScale) ++ "px"),
             height (String.fromFloat (suggestedHeight * heightScale) ++ "px"),
             stroke "#a9a9a9", 
             fill "#eee"
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
      , drawText (xRight + offsetX, yTop + 20) box.name "left"
      , drawText (xRight + offsetX, yTop + 40) ("CPU: " ++ String.fromFloat box.width) "left"
      , drawText (xRight + offsetX, yTop + 60) ("Mem: " ++ String.fromFloat box.height) "left"
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
            [ Svg.text text]

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
     

calculateBoxArea: Box -> Float
calculateBoxArea box =
    box.width * box.height