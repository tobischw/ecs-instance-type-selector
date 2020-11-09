module App.Cluster exposing (view)

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

view : Int -> Configuration.Cluster -> Html msg
view id cluster =
    Card.config []
        |> Card.header [] [ text ("Cluster Settings") ]
        |> Card.view
