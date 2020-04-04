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
            { model | services = Dict.update id (Maybe.map (\service -> { service | scalingTarget = Util.toInt value })) model.services }


view : Int -> Configuration.Service -> Html Msg
view id service =
    Card.config []
        |> Card.header [] [ text service.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Util.viewFormRowSlider "Scaling Target" ((String.fromInt <| service.scalingTarget) ++ "%") service.scalingTarget 0 100 1 (UpdateScalingTarget id)
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Packing Strategy" ]
                        , Form.col [ Col.sm9, Col.attrs [ class "mt-" ] ]
                            []
                        ]
                    ]
            ]
        |> Card.view
