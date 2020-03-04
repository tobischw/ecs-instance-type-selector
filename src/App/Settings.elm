module App.Settings exposing (view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Html exposing (..)
import Html.Attributes exposing (..)

view : Html msg
view =
    Card.config []
        |> Card.header [] [ text "Global Settings" ]
        |> Card.block []
            [ Block.custom <|
                div [] [ text "Global settings "]
            ]
        |> Card.view