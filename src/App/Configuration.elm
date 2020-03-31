module App.Configuration exposing (Cluster, Container, Model, Msg(..), Service, init, update, view)

import App.Util as Util
import App.Constants exposing (allRegions, RegionRecord)
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
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
    }


type Msg
    = AddService
    | AddContainer Int


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddService ->
            let
                    id = Dict.size model.services
            in
            --{ model | services = model.services |> Dict.insert id (Service name 50 (Multiselect.initModel (List.map (\region -> ( region.regionCode, region.displayName )) allRegions) "A") 50 Dict.empty), newServiceName = "", newServiceModal = Modal.hidden }
            model

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

view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Configuration"
        , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ]{-, Button.onClick ShowModal -}] [ FeatherIcons.plus |> FeatherIcons.toHtml [], text "Add Cluster" ]
        , ListGroup.custom (viewClusters model)
        , hr [] []
        , ListGroup.custom
            [ simpleListItem "Global Settings" FeatherIcons.settings [ href "../../settings" ]
            , simpleListItem "Export as JSON" FeatherIcons.share [ href "#" ]
            , simpleListItem "Load JSON" FeatherIcons.download [ href "#" ]
            ]
        ]

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
        , viewServices model (getServices id model.services) 
        ]
            

getServices : Int -> Services -> Services
getServices clusterId services =
    let
        associateService _ service =
            if service.clusterId == clusterId then
                Just service
            else 
                Nothing
    in
        services |> filterMap associateService


viewServices : Model -> Services -> List (ListGroup.CustomItem Msg)
viewServices model services =
    List.concatMap (viewServiceItem model) (Dict.toList services)


viewServiceItem : Model -> ( Int, Service ) -> List (ListGroup.CustomItem Msg)
viewServiceItem model serviceTuple =
    let
        id =
            first serviceTuple

        service =
            second serviceTuple
    in
    List.concat 
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("/service/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text service.name ] , span [ class "" ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []]
                    ]
                ]
          ]
        , viewTaskItem id 
        , viewContainers model (getContainers id model.containers)
        ]
    
viewTaskItem : Int -> List (ListGroup.CustomItem Msg)
viewTaskItem id =
 [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "50px", href ("/task/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Tasks" ]
                    , span [] [ Button.button [ Button.outlineSuccess, Button.small, Button.onClick (AddContainer id) ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [], text "Container" ] ]
                    ]
                ] ]
          

getContainers : Int -> Containers -> Containers
getContainers serviceId containers =
    let
        associateContainer _ container =
            if container.serviceId == serviceId then
                Just container
            else 
                Nothing
    in
        containers |> filterMap associateContainer

viewContainers : Model -> Containers -> List (ListGroup.CustomItem Msg)
viewContainers model containers =
    List.map viewContainerItem (Dict.toList model.containers)

viewContainerItem : ( Int, Container ) -> ListGroup.CustomItem Msg
viewContainerItem containerTuple =
    let
        id = first containerTuple
        container =
            second containerTuple
    in
    simpleListItem container.name FeatherIcons.box  [ href ("/container/" ++ String.fromInt id), style "padding-left" "60px" ]



simpleListItem : String -> FeatherIcons.Icon -> List (Html.Attribute Msg) -> ListGroup.CustomItem Msg
simpleListItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ icon |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text label ]
