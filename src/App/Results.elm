module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances, isSuitableInstance)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li)
import Html.Attributes exposing (class, style)
import Pack exposing (..)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Pixels exposing (..)
import Quantity exposing (Quantity(..))
import Svg exposing (Svg, g, rect, svg, text_)
import Svg.Attributes exposing (alignmentBaseline, fontSize, fill, height, stroke, textAnchor, transform, width, x, y)
import App.Configuration exposing (Daemons)

widthScale : Float
widthScale = 0.15

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
    , daemonBoxes: List DaemonBOX
    }

type alias DaemonBOX = 
    { name: String
    , height: Quantity Float Pixels
    , width: Quantity Float Pixels
    }


view : Model -> Html msg
view model =
    div [ class "pt-1", class "px-3" ]
        [ Util.viewColumnTitle
            "Results"
        , hr [] []
        , viewResultsForService model
        ]


pxToString : Quantity Float Pixels -> String
pxToString pixel =
    String.fromFloat (inPixels pixel)


convertDaemonToBox : Configuration.Daemon -> DaemonBOX
convertDaemonToBox daemon =
    { name = daemon.name
    , height = pixels (toFloat daemon.memory)
    , width = pixels (toFloat daemon.cpuShare)
    }


convertContainerToBox : Model -> (Int, Configuration.Container) -> { data : ContainerData, height : Quantity Float Pixels, width : Quantity Float Pixels }
convertContainerToBox model (containerId, container) =
    let 
        daemons = Daemon.daemonsForContainer model.configuration.daemons containerId
    in
        { data = { name = container.name, color = container.color, daemonBoxes = List.map (\tupl -> convertDaemonToBox (Tuple.second tupl)) daemons }
        , height = pixels (toFloat container.memory)
        , width = pixels (toFloat container.cpuShare)
        }


viewResultsForService : Model -> Html msg
viewResultsForService model =
    let
        convertedContainers =
            List.map (convertContainerToBox model) (Dict.toList model.configuration.containers)
        packingData =
            convertedContainers
                |> Pack.pack { powerOfTwoSize = False, spacing = Quantity.zero }
        vcpu = inPixels packingData.width |> round
        share = round <| toFloat vcpu / 1024
        memory = inPixels packingData.height |> round
        showSuggestions = Dict.isEmpty model.configuration.containers == False

        (topSuggestion, remainingSuggestions) = 
            if showSuggestions then
                Instances.findOptimalSuggestions model.instances share memory 5
            else
               (Instances.defaultInstance, []) 

        topWidth = Quantity (toFloat topSuggestion.vCPU * 1024)
        topHeight = Quantity (toFloat topSuggestion.memory)
    in
    div []
        [ 
          if showSuggestions then 
          div [] [  
          strong [] [ text "Instance:"]
        , br [] []
        , svg
            [ width (Quantity.multiplyBy widthScale topWidth |> pxToString)
            , height (Quantity.multiplyBy heightScale topHeight |> pxToString)
            , style "background-color" "#eee"
            , style "border" "thin solid #a9a9a9"
            ]
            (List.concatMap viewChartSlice packingData.boxes)
        , br [] []
        , text ("Ideal CPU share: " ++ String.fromInt share ++ " (Something still wrong with this)")
        , br [] []
        , text ("Ideal memory: " ++ Util.formatMegabytes memory) 
        , hr [] []
        , strong [] [ text "Top Suggestion:"]
        , viewInstance topSuggestion
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
        (List.map viewInstance instances)


viewInstance : Instance -> Html msg
viewInstance instance =
    div [ style "margin-top" "10px"] [
        Card.config []
        |> Card.block []
            [ Block.text [] [ strong [] [ text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> Util.formatMegabytes) ++ " (" ++ instance.operatingSystem ++")") ] ] 
            , Block.custom <| div [] (List.map viewPrice instance.prices)
            ]
        |> Card.view
    ]


viewPrice : Instances.Price -> Html msg
viewPrice price =
    case price of
        Instances.Upfront value ->
            span [] [ text <| "OnDemand: $" ++ String.fromFloat value ++ " upfront" ]

        Instances.Hourly value ->
            span [] [ text <| "OnDemand: $" ++ String.fromFloat value ++ "/hr" ]


viewChartSlice : PackedBox Float Pixels ContainerData -> List (Svg msg)
viewChartSlice box =
    [ g
        [ transform ("translate(" ++ pxToString (Quantity.multiplyBy widthScale box.x) ++ ", " ++ pxToString (Quantity.multiplyBy heightScale box.y) ++ ")")
        ]
        ([ rect
            [ x "0"
            , y "0"
            , width (pxToString (Quantity.multiplyBy widthScale box.width))
            , height (pxToString (Quantity.multiplyBy heightScale box.height))
            , stroke "#a9a9a9"
            , fill box.data.color 
            ] []
        {--, Svg.text_
            [ x (pxToString (Quantity.half (Quantity.multiplyBy widthScale box.width)))
            , y (pxToString (Quantity.half (Quantity.multiplyBy heightScale box.height)))
            , textAnchor "middle"
            , alignmentBaseline "central"
            ]
            [ Svg.text box.data.name]
        --}]
        ++ List.map viewDaemonSlice box.data.daemonBoxes)
    ]

viewDaemonSlice : DaemonBOX -> Svg msg 
viewDaemonSlice daemon =
    g [] [ 
        rect [
          x "0"
        , y "0"
        , width (pxToString (Quantity.multiplyBy widthScale daemon.width))
        , height (pxToString (Quantity.multiplyBy heightScale daemon.height))
        , stroke "red"
        , fill "orange"
        ] [] 
        , Svg.text_
            [ x (pxToString (Quantity.half (Quantity.multiplyBy widthScale daemon.width)))
            , y (pxToString (Quantity.half (Quantity.multiplyBy heightScale daemon.height)))
            , textAnchor "middle"
            , fontSize "12px"
            , alignmentBaseline "central"
            ]
            [ Svg.text daemon.name]
        ]
      