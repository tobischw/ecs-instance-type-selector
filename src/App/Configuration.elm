module App.Configuration exposing (Container, ContainerProps(..), Model, Msg(..), RegionRecord, Service, allRegions, init, subscriptions, update, updateContainers, view)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Multiselect
import Tuple exposing (first, second)
import FeatherIcons

testServices : Dict Int Service
testServices =
    Dict.fromList [ ( 0, Service "Service A" 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 (Dict.fromList [ ( 0, Container "Container 1" 50 50 50 100) ]) ) ]


init : Model
init =
    { services = testServices
    , newServiceModal = Modal.hidden
    , newServiceName = ""
    , tabState = Tab.initialState
    }


type alias Services =
    Dict Int Service


type alias Model =
    { services : Services
    , newServiceModal : Modal.Visibility
    , newServiceName : String
    , tabState : Tab.State
    }


type Msg
    = AddService
    | AddContainer Int
    | CloseModal
    | ShowModal
    | ChangeNewServiceName String
    | TabMsg Tab.State


type alias Service =
    { name : String
    , scalingTarget : Int
    , regions : Multiselect.Model
    , taskTotalMemory : Int
    , containers : Dict Int Container
    }


type alias Container =
    { name : String
    , cpuShare : Int
    , memory : Int
    , ioops : Int
    , bandwidth: Int
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg

type ContainerProps
    = CPUShare Int
    | Name String
    | Memory Int
    | Ioops Int
    | Bandwidth Int

updateContainers : Int -> Int -> Dict Int Service -> ContainerProps -> Dict Int Container
updateContainers serviceId containerId services containerUpdate =
    let
        maybeService =
            Dict.get serviceId services
    in
    case maybeService of
        Just service ->
            case containerUpdate of
                CPUShare num ->
                    Dict.update containerId (Maybe.map (\container -> { container | cpuShare = num })) service.containers

                Name newName ->
                    Dict.update containerId (Maybe.map (\container -> { container | name = newName })) service.containers

                Memory newMem ->
                    Dict.update containerId (Maybe.map (\container -> { container | memory = newMem })) service.containers

                Ioops newIoops ->
                    Dict.update containerId (Maybe.map (\container -> { container | ioops = newIoops })) service.containers

                Bandwidth newBandwidth ->
                    Dict.update containerId (Maybe.map (\container -> { container | bandwidth = newBandwidth })) service.containers

        Nothing ->
            Dict.fromList [ ( 0, Container "Invalid Container" 50 50 50 50 ) ]


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
            -- These long lines need to be split up
            { model | services = model.services |> Dict.insert id (Service name 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 Dict.empty), newServiceName = "", newServiceModal = Modal.hidden }

        CloseModal ->
            { model | newServiceModal = Modal.hidden, newServiceName = "" }

        ShowModal ->
            { model | newServiceModal = Modal.shown }

        ChangeNewServiceName newName ->
            { model | newServiceName = newName }

        AddContainer serviceId ->
            let
                maybeService =
                    Dict.get serviceId model.services
            in
                case maybeService of
                    Just service ->
                        { model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Dict.insert (Dict.size service.containers) (Container ("Container " ++ String.fromInt (Dict.size service.containers + 1)) 0 0 0 0) service.containers })) model.services }
                    Nothing ->
                        model
        TabMsg state ->
            { model | tabState = state }





viewServices : Services -> List (ListGroup.CustomItem Msg)
viewServices services =
    List.concatMap viewService (Dict.toList services)


viewService : ( Int, Service ) -> List (ListGroup.CustomItem Msg)
viewService serviceWithId =
    let
        serviceId =
            first serviceWithId

        service =
            second serviceWithId
    in
    List.concat
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, class "service-item", href ("/service/" ++ String.fromInt serviceId) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text service.name ] , span [ class "" ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []]
                    ]
                ]
          ]
        , [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("/task/" ++ String.fromInt serviceId) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Tasks" ]
                    , span [] [ Button.button [ Button.outlineSuccess, Button.small, Button.onClick (AddContainer serviceId) ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [], text "Container" ] ]
                    ]
                ]
          ]
        , List.map (viewContainer serviceId) (Dict.toList service.containers)
        ]


viewContainer : Int -> ( Int, Container ) -> ListGroup.CustomItem Msg
viewContainer serviceId containerWithId =
    let
        container =
            second containerWithId
    in
    simpleListItem container.name FeatherIcons.box  [ href ("/container/" ++ String.fromInt serviceId ++ "/" ++ String.fromInt (first containerWithId)), style "padding-left" "60px" ]


viewNewServiceModal : Model -> Html Msg
viewNewServiceModal model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "New Service" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.md ]
                        [ Form.group []
                            [ Form.label [] [ text "Name:" ]
                            , Input.text
                                [ Input.value model.newServiceName
                                , Input.onInput ChangeNewServiceName
                                , Input.attrs [ placeholder "Service Name" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , Grid.row []
                [ Grid.col
                    [ Col.sm ]
                    [ viewConfigureNewServiceModel model ]
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


viewConfigureNewServiceModel : Model -> Html Msg
viewConfigureNewServiceModel model =
    Tab.config TabMsg
        |> Tab.withAnimation
        |> Tab.items
            [ Tab.item
                { id = "addContainer"
                , link = Tab.link [] [ viewAddContainerTabHeader ]
                , pane =
                    Tab.pane [ Spacing.mt1 ]
                        [ h4 [] [ text "New Container" ]
                        , hr [] []
                        , viewAddContainerTabBody
                        ]
                }
            ]
        |> Tab.view model.tabState


viewAddContainerTabHeader : Html Msg
viewAddContainerTabHeader =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.md8 ] [ text "Add Container" ]
            , Grid.col [ Col.md4 ] [ Button.button [ Button.small, Button.danger ] [ text "Delete" ] ]
            ]
        ]


viewAddContainerTabBody : Html Msg
viewAddContainerTabBody =
    Form.form []
        [ Form.group []
            [ Form.label [ for "containerName" ] [ text "Container Name:" ]
            , Input.text [ Input.attrs [ placeholder "Oba#123" ] ]
            ]
        ]


simpleListItem : String -> FeatherIcons.Icon -> List (Html.Attribute Msg) -> ListGroup.CustomItem Msg
simpleListItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ icon |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text label ]


view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Configuration"
        , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ], Button.onClick ShowModal ] [ FeatherIcons.plus |> FeatherIcons.toHtml [], text "Add Service" ]
        , ListGroup.custom (viewServices model.services)
        , hr [] []
        , ListGroup.custom
            [ simpleListItem "Global Settings" FeatherIcons.settings [ href "../../settings" ]
            , simpleListItem "Export as JSON" FeatherIcons.share [ href "#" ]
            , simpleListItem "Load JSON" FeatherIcons.download [ href "#" ]
            ]
        , viewNewServiceModal model
        ]
