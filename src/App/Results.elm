module App.Results exposing (..)

import App.Configuration as Configuration
import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import Color
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text)
import Html.Attributes exposing (class, style)
import Pack exposing (..)
import Pixels exposing (..)
import Quantity exposing (Quantity(..))
import Svg exposing (Svg, g, rect, svg, text_)
import Svg.Attributes exposing (alignmentBaseline, fill, height, stroke, textAnchor, transform, width, x, y)


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
        , hr [] []
        , Util.viewColumnTitle
            "Suggested Instance Types"
        , span [ class "text-muted" ] [ text "Suggestions do not work yet, but it would look like:" ]
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
            [ width (packingData.width |> pxToString)
            , height (packingData.height |> pxToString)
            , style "background-color" "#eee"
            , style "border" "thin solid #a9a9a9"
            ]
            (List.concatMap viewChartItem packingData.boxes)
        , br [] []
        , text ("Ideal width (cpu): " ++ (packingData.width |> pxToString))
        , br [] []
        , text ("Ideal height (mem): " ++ (packingData.height |> pxToString))
        ]


viewChartItem : PackedBox Float Pixels ContainerData -> List (Svg Msg)
viewChartItem box =
    [ g
        [ transform ("translate(" ++ pxToString box.x ++ ", " ++ pxToString box.y ++ ")")
        ]
        [ rect
            [ x "0"
            , y "0"
            , width (pxToString box.width)
            , height (pxToString box.height)
            , stroke "#a9a9a9"
            , fill "#c6ecff"
            ]
            []
        , Svg.text_
            [ x (pxToString (Quantity.half box.width))
            , y (pxToString (Quantity.half box.height))
            , textAnchor "middle"
            , alignmentBaseline "central"
            ]
            [ Svg.text box.data.name ]
        ]
    ]
