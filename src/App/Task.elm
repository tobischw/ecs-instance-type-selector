module App.Task exposing (view)

import App.Container as Container
import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Select as Select
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)


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



view : Html msg
view =
    div []
        [ Card.config []
            |> Card.header [] [ text "Task A" ]
            |> Card.block []
                [ Block.custom <|
                    div []
                        [ Form.row []
                            [ Form.col []
                                [ 
                                    Select.select []
                                        (List.map (\ c -> Select.item [] [text c.displayName]) allRegions)
                                ]
                            ]
                        , Form.row []
                            [ Form.colLabel [ Col.sm3 ] [ text "Total Memory" ]
                            , Form.col [ Col.sm9 ]
                                [ input [ type_ "range", class "form-control-range disabled", disabled True ] []
                                , Form.help [] [ text "-- MiB · Memory limit of all containers in this task for scaling purposes" ]
                                ]
                            ]
                        ]
                ]
            |> Card.view
        ]
