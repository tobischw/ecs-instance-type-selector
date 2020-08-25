module App.Container exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    Configuration.Model


type Msg
    = UpdateCPUShare Int String
    | UpdateMemory Int String
    | UpdateIoops Int String
    | UpdateEBS Int Bool
    | UpdateBandwidth Int String
    | ToggleMoreMemory Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCPUShare id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | cpuShare = Util.toInt value })) model.containers }

        UpdateMemory id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | memory = Util.toInt value })) model.containers }

        UpdateIoops id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | ioops = Util.toInt value })) model.containers }

        UpdateEBS id checked ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | useEBS = checked })) model.containers }

        UpdateBandwidth id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | bandwidth = Util.toInt value })) model.containers }

        ToggleMoreMemory id checked ->
            { model
                | containers =
                    Dict.update id
                        (Maybe.map
                            (\container ->
                                { container
                                    | showExtraMemory = checked
                                    , memory =
                                        if checked then
                                            container.memory

                                        else
                                            32000
                                }
                            )
                        )
                        model.containers
            }


determineMaxContainerMemory : Bool -> Int
determineMaxContainerMemory useMoreMem =
    if useMoreMem then
        24576000

    else
        32000


determineContainerMemStep : Bool -> Int
determineContainerMemStep extraMemEnabled =
    if extraMemEnabled then
        1000

    else
        250



-- this function feels odd


viewMemoryLabel : Int -> String
viewMemoryLabel memoryInMB =
    if memoryInMB < 999 then
        String.fromInt memoryInMB ++ " MiB"

    else if memoryInMB < 999999 then
        String.fromFloat (toFloat memoryInMB / 1000) ++ " GiB"

    else
        String.fromFloat (toFloat memoryInMB / 1000000) ++ " TiB"


view : Int -> Configuration.Container -> Html Msg
view id container =
    Card.config []
        |> Card.header [] [ text container.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    -- these Util calls are a bit odd, but do make the code a bit more organized.
                    [ Util.viewFormRowSlider "CPU Share" ((String.fromInt <| container.cpuShare) ++ "/1024 CPU Share") container.cpuShare 0 1024 10 (UpdateCPUShare id)
                    , hr [] []
                    , Util.showIf (container.memory >= 32000 || container.showExtraMemory) (Util.viewFormCheckbox "Show more memory options" "" container.showExtraMemory (ToggleMoreMemory id))
                    , Util.viewFormRowSlider "Memory" (viewMemoryLabel container.memory) container.memory 250 (determineMaxContainerMemory container.showExtraMemory) (determineContainerMemStep container.showExtraMemory) (UpdateMemory id)
                    , hr [] []
                    , Util.viewFormCheckbox "Use Elastic Block Storage" "" container.useEBS (UpdateEBS id)
                    , Util.viewFormRowSlider "IOOPs" ((String.fromInt <| container.ioops) ++ " MiB/sec") container.ioops 4750 19000 1000 (UpdateIoops id)
                    , hr [] []
                    , Util.viewFormRowSlider "Bandwidth" ((String.fromInt <| container.bandwidth) ++ " GiB/sec") container.bandwidth 1 25 1 (UpdateBandwidth id)
                    ]
            ]
        |> Card.view
