module App.Cluster exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Tuple exposing (first, second)


type alias Model =
    Configuration.Model


type Msg
    = UpdateCluster


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCluster ->
            model


view : Int -> Configuration.Cluster -> Html Msg
view id cluster =
    Card.config []
        |> Card.header [] [ text cluster.name ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Test Field" ]
                        , Form.col [ Col.sm9 ]
                            []
                        ]
                    ]
            ]
        |> Card.view
