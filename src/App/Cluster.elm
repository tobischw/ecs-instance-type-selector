module App.Cluster exposing (..)

import App.Configuration as Configuration exposing (RegionRecord, allRegions)
import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Multiselect
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Tuple exposing (first, second)

placeholder =
    ""