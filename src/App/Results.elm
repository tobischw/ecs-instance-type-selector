module App.Results exposing (..)

import App.Configuration as Configuration
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text)
import Html.Attributes exposing (class, style)
import Pack exposing (..)
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
            "Packing"
        , hr [] []
        , viewResults model
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


viewResults : Model -> Html msg
viewResults model =
    let
        convertedContainers =
            List.map convertContainerToBox (Dict.values model.configuration.containers)

        packingData =
            convertedContainers
                |> Pack.pack { powerOfTwoSize = False, spacing = Quantity.zero }

        vCPUs = inPixels packingData.width |> round

        memory = inPixels packingData.height |> round

        suggestions = model.instances |> List.filter (suitableInstance memory) |> List.sortBy .memory

    in
    div []
        [ svg
            [ width (Quantity.multiplyBy widthScale packingData.width |> pxToString)
            , height (Quantity.multiplyBy heightScale packingData.height |> pxToString)
            , style "background-color" "#eee"
            , style "border" "thin solid #a9a9a9"
            ]
            (List.concatMap viewChartItem packingData.boxes)
        , br [] []
        , strong [] [ text "Instance Type for Service:"]
        , br [] []
        , text ("Ideal CPU share: " ++ String.fromInt vCPUs ++ " vCPUs")
        , br [] []
        , text ("Ideal memory: " ++ String.fromInt memory ++ " MiB") -- use Util.formatMegabytes for this
        , hr [] []
        , viewSuggestions suggestions
        ]


suitableInstance : Int -> Instance -> Bool
suitableInstance memory instance =
    instance.memory >= memory


viewSuggestions : Instances -> Html msg 
viewSuggestions instances = 
    div []
        (List.map viewInstance instances)

viewInstance : Instance -> Html msg
viewInstance instance =
    div [] [
        text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> String.fromInt) ++ "MiB")
    ]

viewChartItem : PackedBox Float Pixels ContainerData -> List (Svg msg)
viewChartItem box =
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
