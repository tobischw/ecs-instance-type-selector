module App.Task exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    Configuration.Model


type Msg
    = UpdateTotalMemory Int Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTotalMemory serviceId id value ->
            case String.toInt value of
                Just i ->
                    -- This needs to be either cleaned up or split into different function for easier re-use
                    --{ model | services = Dict.update id (Maybe.map (\scalingTarget -> { scalingTarget | scalingTarget = i })) model.services }
                    model
                Nothing ->
                    model

view : Int -> Int -> Configuration.Task -> Html msg
view serviceId id task =
    div []
        [ Card.config []
            |> Card.header [] [ text task.name ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Total Memory" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range disabled", value <| String.fromInt task.totalMemory ] []
                            , Form.help [] [ text "-- MiB · Memory limit of all containers in this task for scaling purposes" ]
                            ]
                        ]
                ]
            |> Card.view
        ]
