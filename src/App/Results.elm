module App.Results exposing (..)

import App.Configuration as Configuration
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
import Svg.Attributes exposing (alignmentBaseline, fill, height, stroke, textAnchor, transform, width, x, y)

widthScale : Float
widthScale = 0.35

heightScale : Float
heightScale = 0.0175


type alias Model = 
    {
     configuration : Configuration.Model
    , instances: Instances.Model
    }


type alias ContainerData =
    { name : String
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



convertContainerToBox : Configuration.Container -> { data : ContainerData, height : Quantity Float Pixels, width : Quantity Float Pixels }
convertContainerToBox container =
    { data = { name = container.name }
    , height = pixels (toFloat container.memory)
    , width = pixels (toFloat container.cpuShare)
    }


viewResultsForService : Model -> Html msg
viewResultsForService model =
    let
        convertedContainers =
            List.map convertContainerToBox (Dict.values model.configuration.containers)

        packingData =
            convertedContainers
                |> Pack.pack { powerOfTwoSize = False, spacing = Quantity.zero }

        vcpu = inPixels packingData.width |> round

        memory = inPixels packingData.height |> round

        showSuggestions = Dict.isEmpty model.configuration.containers == False
        suggestions = 
            if showSuggestions then
                model.instances |> List.filter (isSuitableInstance vcpu memory) |> List.sortBy .memory |> List.take 3
            else
                []
    in
    div []
        [ 
          if showSuggestions then 
          div [] [  
          strong [] [ text "Service:"]
        , br [] []
        , svg
            [ width (Quantity.multiplyBy widthScale packingData.width |> pxToString)
            , height (Quantity.multiplyBy heightScale packingData.height |> pxToString)
            , style "background-color" "#eee"
            , style "border" "thin solid #a9a9a9"
            ]
            (List.concatMap viewChartSlice packingData.boxes)
        , br [] []
        , text ("Ideal CPU share: " ++ String.fromInt vcpu ++ " vCPUs")
        , br [] []
        , text ("Ideal memory: " ++ String.fromInt memory ++ " MiB") -- use Util.formatMegabytes for this
        , hr [] []
        , strong [] [ text "Top 3 Suggestions:"]
        , br [] []
        , viewSuggestions suggestions 
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
            [ Block.text [] [ text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> String.fromInt) ++ "MiB") ] ]
        |> Card.view
    ]

viewChartSlice : PackedBox Float Pixels ContainerData -> List (Svg msg)
viewChartSlice box =
    [ g
        [ transform ("translate(" ++ pxToString (Quantity.multiplyBy widthScale box.x) ++ ", " ++ pxToString (Quantity.multiplyBy heightScale box.y) ++ ")")
        ]
        [ rect
            [ x "0"
            , y "0"
            , width (pxToString (Quantity.multiplyBy widthScale box.width))
            , height (pxToString (Quantity.multiplyBy heightScale box.height))
            , stroke "#a9a9a9"
            , fill "#c6ecff"
            ]
            []
        , Svg.text_
            [ x (pxToString (Quantity.half (Quantity.multiplyBy widthScale box.width)))
            , y (pxToString (Quantity.half (Quantity.multiplyBy heightScale box.height)))
            , textAnchor "middle"
            , alignmentBaseline "central"
            ]
            [ Svg.text box.data.name ]
        ]
    ]
