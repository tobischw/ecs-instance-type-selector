module App.Service exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)



-- This seems a bit messy, but technically allowed since we end up w/ "global" state


type alias Model =
    Configuration.Model


type Msg
    = UpdateScalingTarget Int String


-- find a better way to do this!
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScalingTarget id value ->
            case String.toInt value of
                Just i ->
                    { model | services = Dict.update id (Maybe.map (\scalingTarget -> { scalingTarget | scalingTarget = i })) model.services }
                Nothing ->
                    model


view : Int -> Configuration.Service -> Html Msg
view id service =
    Card.config []
        |> Card.header [] [ text service.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Test Field" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt service.scalingTarget, onInput (UpdateScalingTarget id) ] []
                            , Form.help [] [ text (String.fromInt service.scalingTarget ++ "% utilization") ]
                            ]
                        ]
                    ]
            ]
        |> Card.view
