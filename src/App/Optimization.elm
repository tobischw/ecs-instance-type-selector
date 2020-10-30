module App.Optimization exposing (..)
import App.Configuration exposing (Containers)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)

type alias ChartBox =
    { data : Int
    , width: Quantity Float Pixels
    , height: Quantity Float Pixels
    }

buildDiagonalGraph : Containers -> List ChartBox
buildDiagonalGraph containers =
    let
        sortedContainers = containers
    in
    []

