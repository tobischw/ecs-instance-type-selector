module App.Task exposing (view)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)

view : Html msg
view =
    Card.config []
        |> Card.header [] [ text "Task A" ]
        |> Card.block []
            [ Block.text [] [ text "This is some text within a card block." ] ]
        |> Card.view

    