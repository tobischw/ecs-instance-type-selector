module App.Service exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Util as Util 
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onChange)


type alias Model =
    Configuration.Model


type Msg
    = UpdateScalingTarget Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScalingTarget id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | scalingTarget = Util.toInt value})) model.services }


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
