module App.Task exposing (Model, Msg(..), update, view)

import App.Container as Container
import App.Util as Util
import Bootstrap.Button as Button
import App.Configuration as Configuration
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Select as Select
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias RegionRecord =
    { regionCode : String
    , displayName : String
    , regionName : String
    }



-- https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.partial.html


allRegions : List RegionRecord
allRegions =
    [ RegionRecord "us" "US East (Ohio)" "us-east-2"
    , RegionRecord "us" "US East (N. Virginia)" "us-east-1"
    , RegionRecord "us" "US West (N. California)" "us-west-1"
    , RegionRecord "us" "US West (Oregon)" "us-west-2"
    , RegionRecord "ap" "Asia Pacific (Hong Kong)" "ap-east-1"
    , RegionRecord "ap" "Asia Pacific (Mumbai)" "ap-south-1"
    , RegionRecord "ap" "Asia Pacific (Osaka-Local)" "ap-northeast-3"
    , RegionRecord "ap" "Asia Pacific (Seoul)" "ap-northeast-2"
    , RegionRecord "ap" "Asia Pacific (Singapore)" "ap-southeast-1"
    , RegionRecord "ap" "Asia Pacific (Sydney)" "ap-southeast-2"
    , RegionRecord "ap" "Asia Pacific (Tokyo)" "ap-northeast-1"
    , RegionRecord "ca" "Canada (Central)" "ca-central-1"
    , RegionRecord "cn" "China (Beijing)" "cn-north-1"
    , RegionRecord "cn" "China (Ningxia)" "cn-northwest-1"
    , RegionRecord "eu" "Europe (Frankfurt)" "eu-central-1"
    , RegionRecord "eu" "Europe (Ireland)" "eu-west-1"
    , RegionRecord "eu" "Europe (London)" "eu-west-2"
    , RegionRecord "eu" "Europe (Paris)" "eu-west-3"
    , RegionRecord "eu" "Europe (Stockholm)" "eu-north-1"
    , RegionRecord "me" "Middle East (Bahrain)" "me-south-1"
    , RegionRecord "sa" "South America (Sao Paulo)" "sa-east-1"
    ]

type alias Model =
    Configuration.Model


type Msg
    = UpdateTotalMemory Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTotalMemory id value ->
            case String.toInt value of
                Just i ->
                    -- This is a mess, someone help please!
                    { model | services = Dict.update id (Maybe.map (\task -> { task | task = Configuration.Task i })) model.services }

                Nothing ->
                    model


view : Int -> Configuration.Service -> Html Msg
view serviceId service =
    div []
        [ Card.config []
            |> Card.header [] [ text (service.name ++ " · Task Setup") ]
            |> Card.block []
                [ Block.custom <|
                    div [] [
                        span [] [ text "This is where you would select how many tasks + what region they are in"]
                    ]
                ]
            |> Card.view
        , Card.config [ Card.attrs [ class "mt-3" ] ]
            |> Card.header [] [ text (service.name ++ " · Task Configuration") ]
            |> Card.block []
                [ Block.custom <|
                    Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Test Field Task" ]
                        , Form.col [ Col.sm9 ]
                            [ input [ type_ "range", class "form-control-range", value <| String.fromInt service.task.totalMemory, onInput (UpdateTotalMemory serviceId) ] []
                            , Form.help [] [ text (String.fromInt service.task.totalMemory ++ " MiB · Memory limit of all containers in this task for scaling purposes") ]
                            ]
                        ]
                ]
            |> Card.view
        ]
