module App.Task exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dict exposing (Dict)

type alias Model =
    Configuration.Model


type Msg
    = UpdateTotalMemory Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTotalMemory id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update id (Maybe.map (\task -> { task | task = Configuration.Task i })) model.services }
                Nothing ->
                    model

view : Int -> Configuration.Task -> Html Msg
view serviceId task =
    div []
        [ Card.config []
            |> Card.header [] [ text "Tasks" ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Test Field Task" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt task.totalMemory, onInput (UpdateTotalMemory serviceId) ] []
                            , Form.help [] [ text (String.fromInt task.totalMemory ++ " MiB · Memory limit of all containers in this task for scaling purposes") ]
                            ]
                        ]
                ]
            |> Card.view
        ]
