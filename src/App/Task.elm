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
    | UpdateNominalTasks Int String



-- https://elmseeds.thaterikperson.com/elm-multiselect


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMinTasks id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | minTasks = Util.toInt value })) model.services }

        UpdateMaxTasks id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | maxTasks = Util.toInt value })) model.services }

        UpdateNominalTasks id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | nominalTasks = Util.toInt value })) model.services }


view : Int -> Configuration.Service -> Configuration.Containers -> Html Msg
view id service containers =
    div []
        [ Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " - Task Settings") ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormRowSlider "Min. Tasks" ((String.fromInt <| service.minTasks) ++ " Tasks") service.minTasks 1 service.maxTasks 1 (UpdateMinTasks id)
                        , Util.viewFormRowSlider "Nom. Tasks" ((String.fromInt <| service.nominalTasks) ++ " Tasks") service.nominalTasks service.minTasks service.maxTasks 1 (UpdateNominalTasks id)
                        , Util.viewFormRowSlider "Max. Tasks" ((String.fromInt <| service.maxTasks) ++ " Tasks") service.maxTasks service.minTasks 100 1 (UpdateMaxTasks id)
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text "Containers Overview" ]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [ Util.viewFormLabel "Total Memory" "Total memory of all containers in this service combined." ((String.fromFloat <| sumMemory containers * toFloat service.nominalTasks) ++ " GiB")
                        , Util.viewFormLabel "Total CPU Shares" "CPU Shares required for all containers in one task" ((String.fromInt <| sumCPUShare containers * service.nominalTasks) ++ "/1024")
                        , Util.viewFormLabel "Total Bandwidth" "Bandwidth required for all containers in one task" ((String.fromInt <| sumBandwidth containers * service.nominalTasks) ++ " GiB/sec")
                        , Util.viewFormLabel "IO Total" "IO requirements for all containers in one task" (sumIoops service containers)
                        ]
                ]
            |> Card.view
        ]


sumMemory : Configuration.Containers -> Float
sumMemory containers =
    List.sum (List.map (\container -> toFloat container.memory) (Dict.values containers)) / 1000


sumCPUShare : Configuration.Containers -> Int
sumCPUShare containers =
    List.sum (List.map (\container -> container.cpuShare) (Dict.values containers))


sumBandwidth : Configuration.Containers -> Int
sumBandwidth containers =
    List.sum (List.map (\container -> container.bandwidth) (Dict.values containers))


sumIoops : Configuration.Service -> Configuration.Containers -> String
sumIoops service containers =
    let
        -- This feels like a lot of duplicated code
        containersWithEBS =
            List.filter (\container -> container.useEBS == True) (Dict.values containers)

        containersWoEBS =
            List.filter (\container -> container.useEBS == False) (Dict.values containers)

        otherSum =
            List.sum (List.map (\container -> container.ioops) (Dict.values containers))

        someUseEBS =
            List.length containersWithEBS > 0
    in
    if someUseEBS then
        String.fromInt (List.length containersWithEBS) ++ " container(s) using EBS. Total: " ++ String.fromInt otherSum ++ "MiB/sec"

    else
        String.fromInt (otherSum * service.nominalTasks) ++ " MiB/sec"
