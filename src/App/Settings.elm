module App.Settings exposing (Model, Msg(..), init, subscriptions, update, view)

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect


type alias Model =
    { excludedInstances : Multiselect.Model
    }


init : Model
init =
    { excludedInstances = Multiselect.initModel instanceTypes "A"
    }



-- There's a better way to do this...


instanceTypes : List ( String, String )
instanceTypes =
    [ ( "t2", "T2" )
    , ( "m5", "M5" )
    , ( "m4", "M4" )
    , ( "m3", "M3" )
    , ( "c5", "C5" )
    , ( "c4", "C4" )
    , ( "c3", "C3" )
    , ( "x1", "X1" )
    , ( "r4", "R4" )
    , ( "r3", "R3" )
    , ( "p3", "P3" )
    , ( "p2", "P2" )
    , ( "g3", "G3" )
    , ( "f1", "F1" )
    , ( "i3", "I3" )
    ]


type Msg
    = UpdateExcludedInstances Multiselect.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExcludedInstances instancesChangedMessage ->
            let
                ( newExcludedInstances, subCmd, _ ) =
                    Multiselect.update instancesChangedMessage model.excludedInstances
            in
            ( { model | excludedInstances = newExcludedInstances }, Cmd.map UpdateExcludedInstances subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateExcludedInstances <| Multiselect.subscriptions model.excludedInstances


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
                        , Form.help [] [ text "Exclude specific ECS instances. These will be ignored during the cluster optimization calculation." ]
                        ]
                    ]
            ]
        |> Card.view
