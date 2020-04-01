module App.Task exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Constants as Constants exposing (RegionRecord, allRegions)
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


view : Int -> Configuration.Service -> Html Msg
view serviceId service =
    div []
        [ Card.config []
            |> Card.header [] [ text (service.name ++ " · Tasks Setup") ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Regions" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map (UpdateRegions serviceId) <| Multiselect.view service.regions
                            , Form.help [] [ text "Select the regions (for redundancy). Each selection will equal a replicated task." ]
                            ]
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " · Task Arrangment") ]
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
