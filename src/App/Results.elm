module App.Results exposing (..)

import App.Configuration as Configuration
import App.Util as Util
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
    Configuration.Model


type alias ContainerData =
    { name : String
    }


type Msg
    = PlaceHolderMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlaceHolderMsg ->
            model


view : Model -> Html Msg
view model =
    div [ class "pt-1", class "px-3" ]
        [ Util.viewColumnTitle
            "Packing"
        , hr [] []
        , viewChart model
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


viewChart : Model -> Html Msg
viewChart model =
    let
        convertedContainers =
            List.map convertContainerToBox (Dict.values model.containers)

        packingData =
            convertedContainers
                |> Pack.pack { powerOfTwoSize = False, spacing = Quantity.zero }

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
        , text ("Ideal CPU share: " ++ (packingData.width |> pxToString) ++ " vCPUs")
        , br [] []
        , text ("Ideal memory: " ++ (packingData.height |> pxToString) ++ " MiB") -- use Util.formatMegabytes for this
        ]


viewChartItem : PackedBox Float Pixels ContainerData -> List (Svg Msg)
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
