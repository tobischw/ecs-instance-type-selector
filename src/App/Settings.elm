module App.Settings exposing (Model, Msg(..), init, update, view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect


type alias Model =
    { excludedInstances : Multiselect.Model
    }


init : Model
init =
    { excludedInstances = (Multiselect.initModel [ ( "one", "The 1st option" ) ] "A")
    }


instanceTypes : List String
instanceTypes =
    [ "T2"
    , "M5"
    , "M4"
    , "M3"
    , "C5"
    , "C4"
    , "C3"
    , "X1"
    , "R4"
    , "R3"
    , "P3"
    , "P2"
    , "G3"
    , "F1"
    , "I3"
    ]


type Msg
    = UpdateExcludedInstances Multiselect.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateExcludedInstances value ->
            model


view : Model -> Html Msg
view model =
    Card.config []
        |> Card.header [] [ text "Global Settings" ]
        |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Instance Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedInstances <| Multiselect.view model.excludedInstances
                            , Form.help [] [ text "Exclude specific ECS instances" ]
                            ]
                        ]
                ]
        |> Card.view


viewInstanceCheckbox : String -> Html Msg
viewInstanceCheckbox name =
    Form.row []
        [ Form.col [ Col.sm12 ]
            [ Checkbox.checkbox [ Checkbox.id name, Checkbox.checked True ] name ]
        ]
