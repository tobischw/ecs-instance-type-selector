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


view : Int -> Configuration.Service -> Html Msg
view id service =
    div []
        [ Card.config []
            |> Card.header [] [ text (service.name ++ " · Tasks Setup") ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Regions" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map (UpdateRegions id) <| Multiselect.view service.regions
                            , Form.help [] [ text "Select the regions (for redundancy). Each selection will equal a replicated task." ]
                            ]
                        ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text service.name]
            |> Card.block []
                [ Block.custom <|
                    Form.form []
                        [
                            Util.viewFormRowSlider "Min. Tasks" ((String.fromInt <| service.minTasks) ++ " Tasks") service.minTasks 0 100 1 (UpdateMinTasks id)
                            , Util.viewFormRowSlider "Max. Tasks" ((String.fromInt <| service.maxTasks) ++ " Tasks") service.maxTasks 0 100 1 (UpdateMaxTasks id)
                        ]
                ]
            |> Card.view
        ]
