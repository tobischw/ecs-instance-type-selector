module App.Configuration exposing (Cluster, Container, Model, Msg(..), RegionRecord, Service, allRegions, init, subscriptions, update, view)

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
import Dict.Extra exposing (filterMap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Multiselect
import Tuple exposing (first, second)
import FeatherIcons


init : Model
init =
    { clusters = Dict.fromList [ (0, Cluster "Cluster 1") ]
    , services = Dict.fromList [ (0, Service "Service wait" 0 0 (Multiselect.initModel [] "A") 0), (1, Service "Service 2" 0 0 (Multiselect.initModel [] "A") 0)] 
    , containers = Dict.fromList []
    , newServiceModal = Modal.hidden
    , newServiceName = ""
    , tabState = Tab.initialState
    }


type alias Services =
    Dict Int Service

type alias Clusters =
    Dict Int Cluster


type alias Containers =
    Dict Int Container

type alias Model =
    { clusters : Clusters
    , services : Services
    , containers : Containers
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


type alias Cluster =
    {
        name : String
    }

type alias Service =
    { name : String
    , clusterId : Int
    , scalingTarget : Int
    , regions : Multiselect.Model
    , taskTotalMemory : Int
    }


type alias Container =
    { name : String
    , serviceId : Int
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
            --{ model | services = model.services |> Dict.insert id (Service name 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 Dict.empty), newServiceName = "", newServiceModal = Modal.hidden }
            model

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
                        --{ model | services = Dict.update serviceId (Maybe.map (\containers -> { containers | containers = Dict.insert (Dict.size service.containers) (Container ("Container " ++ String.fromInt (Dict.size service.containers + 1)) 0 0 0 0) service.containers })) model.services }
                        model
                    Nothing ->
                        model
        TabMsg state ->
            { model | tabState = state }


viewClusters : Model -> List (ListGroup.CustomItem Msg)
viewClusters model =
    List.concatMap (viewClusterItem model) (Dict.toList model.clusters)


viewClusterItem : Model -> (Int, Cluster) -> List (ListGroup.CustomItem Msg)
viewClusterItem model clusterTuple =
    let
        id = first clusterTuple
        cluster = second clusterTuple
    in
        List.concat
        [
            [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, class "cluster-item", href ("/cluster/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.share2 |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text cluster.name] , span [ class "" ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []]
                    ]
                ]
            ]
        , viewServices (getServices id model.services) 
        ]
            

getServices : Int -> Services -> Services
getServices clusterId services =
    let
        associateService n a =
            if a.clusterId == clusterId then
                let _ = Debug.log "found item" a.name in
                Just a
            else 
                Nothing
    in
        services |> filterMap associateService
    --Dict.fromList [ ( 0, Service "Service" 0 50 (Multiselect.initModel [] "A") 50 ) ] 

viewServices : Services -> List (ListGroup.CustomItem Msg)
viewServices services =
    List.concatMap viewServiceItem (Dict.toList services)


viewServiceItem : ( Int, Service ) -> List (ListGroup.CustomItem Msg)
viewServiceItem serviceTuple =
    let
        serviceId =
            first serviceTuple

        service =
            second serviceTuple
    in
    List.concat -- Todo: these need to be split into separate views or something, too messy
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("/service/" ++ String.fromInt serviceId) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text service.name ] , span [ class "" ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []]
                    ]
                ]
          ]
        , [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "50px", href ("/task/" ++ String.fromInt serviceId) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Tasks" ]
                    , span [] [ Button.button [ Button.outlineSuccess, Button.small, Button.onClick (AddContainer serviceId) ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [], text "Container" ] ]
                    ]
                ]
          ]
        --, List.map (viewContainer serviceId) (Dict.toList service.containers)
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
        , ListGroup.custom (viewClusters model)
        , hr [] []
        , ListGroup.custom
            [ simpleListItem "Global Settings" FeatherIcons.settings [ href "../../settings" ]
            , simpleListItem "Export as JSON" FeatherIcons.share [ href "#" ]
            , simpleListItem "Load JSON" FeatherIcons.download [ href "#" ]
            ]
        , viewNewServiceModal model
        ]
