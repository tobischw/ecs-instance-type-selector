module App.Task exposing (Model, Msg(..), update, view)

import App.Container as Container
import App.Configuration as Configuration exposing (RegionRecord)
import App.Util as Util
import Bootstrap.Button as Button
import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Multiselect
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Select as Select
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Tuple exposing (first, second)


-- https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.partial.html

type alias Model =
    Configuration.Model


type Msg
    = UpdateTotalMemory Int String
    | UpdateRegions Int Multiselect.Msg


-- https://elmseeds.thaterikperson.com/elm-multiselect
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTotalMemory id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update id (Maybe.map (\taskTotalMemory -> { taskTotalMemory | taskTotalMemory = i })) model.services }

                Nothing ->
                    model

        UpdateRegions id regs ->
            let 
                (regsModel, _, _) =
                     let
                        maybeService = Dict.get id model.services
                     in
                        case maybeService of
                            Just service -> 
                                Multiselect.update regs service.regions
                        
                            Nothing ->
                                Multiselect.update regs (Multiselect.initModel [("yeetID", "YEET bb")] "A")
                                 
            in
                { model | services = Dict.update id (Maybe.map (\regions -> { regions | regions = regsModel })) model.services }

view : Int -> Configuration.Service -> Html Msg
view serviceId service =
    div []
        [ Card.config []
            |> Card.header [] [ text (service.name ++ " · Task Setup") ]
            |> Card.block []
                [ Block.custom <|
                    div [] 
                    [ Html.map (UpdateRegions serviceId) <| Multiselect.view service.regions
                    ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " · Task Configuration") ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Test Field Task" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt service.taskTotalMemory, onInput (UpdateTotalMemory serviceId) ] []
                            , Form.help [] [ text (String.fromInt service.taskTotalMemory ++ " MiB · Memory limit of all containers in this task for scaling purposes") ]
                            ]
                        ]
                ]
            |> Card.view
        ]
