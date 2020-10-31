module App.Cluster exposing (Model, Msg(..), update, view)

import App.Configuration as Configuration
import App.Constants as Constants
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
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
    = UpdateClusterRegions Int Multiselect.Msg
    | UpdatePricingFilter Int Configuration.PricingFilter


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Maybe there's a better way to do this
        UpdateClusterRegions id multiSelectMsg ->
            let
                ( regionsModel, _, _ ) =
                    let
                        maybeCluster =
                            Dict.get id model.clusters
                    in
                    case maybeCluster of
                        Just cluster ->
                            Multiselect.update multiSelectMsg cluster.regions

                        Nothing ->
                            Multiselect.update multiSelectMsg Util.initRegionsMultiselect
            in
            { model | clusters = Dict.update id (Maybe.map (\cluster -> { cluster | regions = regionsModel })) model.clusters }

        UpdatePricingFilter id pricingFilter ->
            { model | clusters = Dict.update id (Maybe.map (\cluster -> { cluster | pricingFilter = pricingFilter })) model.clusters }


view : Int -> Configuration.Cluster -> Html Msg
view id cluster =
    Card.config []
        |> Card.header [] [ text (cluster.name ++ " Settings") ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Regions" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map (UpdateClusterRegions id) <| Multiselect.view cluster.regions
                            ]
                        ]
                    ]
            ]
        |> Card.view
