module App.Configuration exposing (Container, ContainerProps(..), Model, Msg(..), RegionRecord, Service, allRegions, init, update, updateContainers, view)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Badge as Badge
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Flex as Flex
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Multiselect
import Tuple exposing (first, second)


testServices : Dict Int Service
testServices =
    Dict.fromList [ ( 0, Service "Service A" 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 (Dict.fromList [ ( 0, Container "Container 1a" 50 50 50 100 ), ( 1, Container "Container 2a" 20 20 20 100 ) ]) ) ]


init : Model
init =
    { services = testServices
    , newServiceModal = Modal.hidden
    , newServiceName = ""
    }


type alias Services =
    Dict Int Service


type alias Model =
    { services : Services
    , newServiceModal : Modal.Visibility
    , newServiceName : String
    }


type Msg
    = AddService
    | CloseModal
    | ShowModal
    | ChangeNewServiceName String


type alias Service =
    { name : String
    , scalingTarget : Int
    , regions : Multiselect.Model
    , taskTotalMemory : Int
    , containers : Dict Int Container
    }


type alias Container =
    { name : String
    , vCPUs : Int
    , memory : Int
    , ioops : Int
    , storage : Int
    }


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


type ContainerProps
    = VCPUS Int
    | Name String
    | Memory Int
    | Ioops Int
    | Storage Int



--               serviceID  containerID allServices containerUpdate -> newContainer


updateContainers : Int -> Int -> Dict Int Service -> ContainerProps -> Dict Int Container
updateContainers serviceId containerId services containerUpdate =
    let
        maybeService =
            Dict.get serviceId services
    in
    case maybeService of
        Just service ->
            case containerUpdate of
                VCPUS num ->
                    Dict.update containerId (Maybe.map (\container -> { container | vCPUs = num })) service.containers

                Name newName ->
                    Dict.update containerId (Maybe.map (\container -> { container | name = newName })) service.containers

                Memory newMem ->
                    Dict.update containerId (Maybe.map (\container -> { container | memory = newMem })) service.containers

                Ioops newIoops ->
                    Dict.update containerId (Maybe.map (\container -> { container | ioops = newIoops })) service.containers

                Storage newSize ->
                    Dict.update containerId (Maybe.map (\container -> { container | storage = newSize })) service.containers

        Nothing ->
            Dict.fromList [ ( 0, Container "Service Id didn't exist" 50 50 50 50 ), ( 1, Container "Yup. v broke." 20 20 20 50 ) ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddService ->
            let
                name =
                    if String.isEmpty model.newServiceName then
                        "Unnamed Service"

                    else
                        model.newServiceName

                id =
                    Dict.size model.services
            in
            { model | services = model.services |> Dict.insert id (Service name 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 Dict.empty), newServiceName = "", newServiceModal = Modal.hidden }

        CloseModal ->
            { model | newServiceModal = Modal.hidden, newServiceName = "" }

        ShowModal ->
            { model | newServiceModal = Modal.shown }

        ChangeNewServiceName newName ->
            { model | newServiceName = newName }



-- rewrite these view functions, use Dict.map?


viewServices : Services -> List (ListGroup.CustomItem msg)
viewServices services =
    List.concatMap viewService (Dict.toList services)


viewService : ( Int, Service ) -> List (ListGroup.CustomItem msg)
viewService serviceWithId =
    let
        serviceId =
            first serviceWithId

        service =
            second serviceWithId
    in
    List.concat
        [ [ listItem service.name "weather-cloudy" [ href ("/service/" ++ String.fromInt serviceId), class "service-item" ]
          ]
        , [ ListGroup.anchor [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("/task/" ++ String.fromInt serviceId) ] ] [ Util.icon "clipboard", text "Tasks", Badge.pillInfo [] [ text "1" ] ] ]
        , List.map (viewContainer serviceId) (Dict.toList service.containers)
        ]


viewContainer : Int -> ( Int, Container ) -> ListGroup.CustomItem msg
viewContainer serviceId containerWithId =
    let
        container =
            second containerWithId
    in
    listItem container.name "archive" [ href ("/container/" ++ String.fromInt serviceId ++ "/" ++ String.fromInt (first containerWithId)), style "padding-left" "60px" ]


viewNewServiceModal : Model -> Html Msg
viewNewServiceModal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "New Service" ]
        |> Modal.body []
            [ Form.form []
                [ Form.group []
                    [ Form.label [] [ text "Name:" ]
                    , Input.text
                        [ Input.value model.newServiceName
                        , Input.onInput ChangeNewServiceName
                        , Input.attrs
                            [ placeholder "Service Name"
                            ]
                        ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick CloseModal
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.success
                , Button.onClick AddService
                ]
                [ text "Add" ]
            ]
        |> Modal.view model.newServiceModal


listItem : String -> String -> List (Html.Attribute msg) -> ListGroup.CustomItem msg
listItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ Util.icon icon, text label ]


view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Configuration"
        , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ], Button.onClick ShowModal ] [ text "Add Service" ]
        , ListGroup.custom (viewServices model.services)
        , hr [] []
        , ListGroup.custom
            [ listItem "Global Settings" "cog" [ href "../../settings" ]
            , listItem "Export as JSON" "eject" [ href "#" ]
            , listItem "Load JSON" "download-outline" [ href "#" ]
            ]
        , viewNewServiceModal model
        ]
