module App.Task exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Constants exposing (allRegions)
import App.Util as Util exposing (viewFormRowSlider)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Multiselect
import Tuple exposing (first, second)


type alias Model =
    Configuration.Model


type Msg
    = UpdateMinTasks Int String
    | UpdateMaxTasks Int String
    | UpdateRegions Int Multiselect.Msg



-- https://elmseeds.thaterikperson.com/elm-multiselect


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMinTasks id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | minTasks = Util.toInt value })) model.services }

        UpdateMaxTasks id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | maxTasks = Util.toInt value })) model.services }

        UpdateRegions id regionChangeMessage ->
            let
                ( newRegionModel, _, _ ) =
                    let
                        maybeService =
                            Dict.get id model.services
                    in
                    case maybeService of
                        Just service ->
                            Multiselect.update regionChangeMessage service.regions

                        Nothing ->
                            Multiselect.update regionChangeMessage (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A")
            in
            { model | services = Dict.update id (Maybe.map (\regions -> { regions | regions = newRegionModel })) model.services }


view : Int -> Configuration.Service -> Configuration.Containers -> Html Msg
view id service containers =
    div []
        [ Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " - Task Settings") ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormRowSlider "Min. Tasks" ((String.fromInt <| service.minTasks) ++ " Tasks") service.minTasks 0 100 1 (UpdateMinTasks id)
                        , Util.viewFormRowSlider "Max. Tasks" ((String.fromInt <| service.maxTasks) ++ " Tasks") service.maxTasks 0 100 1 (UpdateMaxTasks id)
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text "Containers Overview" ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormLabel "Total Memory" "Total memory of all containers in this service combined." ((String.fromFloat <| sumMemory containers) ++ " GiB")
                        , Util.viewFormLabel "Total CPU Shares" "Total number of CPU shares needed for 1 task" ((String.fromInt <| sumCPUShare containers) ++ "/1024")
                        , Util.viewFormLabel "Total CPU Shares" "Total number of CPU shares needed for 1 task" ((String.fromInt <| sumBandwidth containers) ++ " GiB/sec")
                        
                        ]
                ]
            |> Card.view
        ]


sumMemory : Configuration.Containers -> Float
sumMemory containers =
    List.sum (List.map (\container -> toFloat container.memory) (Dict.values containers)) / 1000

sumCPUShare: Configuration.Containers -> Int
sumCPUShare containers = 
    List.sum (List.map (\container -> container.cpuShare) (Dict.values containers))

sumBandwidth: Configuration.Containers -> Int
sumBandwidth containers =
    List.sum (List.map (\container -> container.bandwidth) (Dict.values containers))
