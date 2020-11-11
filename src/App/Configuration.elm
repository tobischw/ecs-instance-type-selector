module App.Configuration exposing (Cluster, Container, Containers, Services, Daemon, Daemons, Model, Msg(..), PackingStrategy(..), Service, Clusters, PricingFilter(..), getContainers, init, update, view)

-- This is probably the only real "messy" file, could do with some refactoring and clean up

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Dict exposing (Dict)
import Dict.Extra exposing (filterMap)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onChange, onEnter)
import Multiselect
import Tuple exposing (first, second)
import Random

init : Model
init =
    { clusters = Dict.fromList [ ( 0, { name = "Cluster" } ) ]
    , services = Dict.fromList [ ] 
    , containers = Dict.fromList [ ] 
    , daemons = Dict.fromList [ ]
    , autoIncrement = 0 -- Set this to 0 once we get rid of sample data
    }


type alias Services =
    Dict Int Service


type alias Clusters =
    Dict Int Cluster


type alias Containers =
    Dict Int Container

type alias Daemons = 
    Dict Int Daemon


type alias Model =
    { clusters : Clusters
    , services : Services
    , containers : Containers
    , daemons: Daemons
    , autoIncrement : Int
    }


type Msg
    = AddCluster
    | AddService Int -- ClusterId
    | AddContainer Int -- ServiceId
    | AddDaemon Int -- ContainerId
    | DeleteContainer Int -- ContainerId
    | DeleteService Int -- ServiceId
    | DeleteCluster Int -- ClusterId
    | DeleteDaemon Int -- DaemonId
    | ChangeContainerName Int String -- ContainerId Name
    | ChangeDaemonName Int String -- DaemonId Name
    | ChangeServiceName Int String -- ServiceId Name
    | ChangeClusterName Int String -- ClusterId Name


type PricingFilter
    = Reserved
    | OnDemand


type alias Cluster =
    { name : String
    }


type alias Service =
    { name : String
    , clusterId : Int
    , scalingTarget : Int
    , packingStrategy : PackingStrategy
    , minTasks : Int
    , maxTasks : Int
    , nominalTasks: Int
    }


type PackingStrategy
    = ByCPUShares
    | ByMemory


type alias Container =
    { name : String
    , color : String
    , serviceId : Int
    , cpuShare : Int
    , memory : Int
    , ioops : Int
    , useEBS : Bool
    , bandwidth : Int
    , showExtraMemory : Bool
    }


type alias Daemon = 
    { memory: Int
    , cpuShare: Int
    , name: String
    , containerId: Int
    }


generateId : Model -> Int
generateId model =
    model.autoIncrement + 1


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCluster ->
            { model | clusters = model.clusters |> Dict.insert model.autoIncrement { name = "Cluster"}, autoIncrement = generateId model }

        AddService clusterId ->
            { model | services = model.services |> Dict.insert model.autoIncrement { name = "Service", clusterId = clusterId, scalingTarget = 0, packingStrategy = ByCPUShares, minTasks = 1, maxTasks = 2, nominalTasks = 1 }, autoIncrement = generateId model }

        AddContainer serviceId ->
            let
                containerId = generateId model
                daemonId = generateId model
                daemons = model.daemons |> Dict.insert daemonId {name = "Daemon", containerId = containerId, cpuShare = 0, memory = 0}
            in
            
            { model | containers = model.containers |> Dict.insert containerId { name = "Container", color = Util.randomColorString (Random.initialSeed model.autoIncrement), serviceId = serviceId, cpuShare = 128, memory = 4000, ioops = 128, useEBS = True, bandwidth = 20, showExtraMemory = False }, daemons = daemons, autoIncrement = containerId + 1 }

        AddDaemon containerId -> 
            { model | daemons = model.daemons |> Dict.insert model.autoIncrement {name = "Daemon", containerId = containerId, cpuShare = 0, memory = 0}, autoIncrement = generateId model }

        DeleteContainer containerId ->
            { model | containers = model.containers |> Dict.remove containerId }

        DeleteDaemon daemonId -> 
            { model | daemons = model.daemons |> Dict.remove daemonId }

        DeleteService serviceId ->
            let
                newModel =
                    { model | containers = model.containers |> Dict.Extra.removeWhen (\_ container -> container.serviceId == serviceId) }
            in
            { newModel | services = model.services |> Dict.remove serviceId }

        DeleteCluster clusterId ->
            -- This mess could use a refactor
            let
                servicesToRemove =
                    model.services |> Dict.filter (\_ service -> service.clusterId == clusterId)

                serviceIdsToRemove =
                    servicesToRemove |> Dict.keys

                newContainers =
                    model.containers |> Dict.Extra.removeWhen (\_ container -> List.length (List.filter (\item -> item == container.serviceId) serviceIdsToRemove) > 0)

                newServices =
                    model.services |> Dict.Extra.removeWhen (\_ service -> service.clusterId == clusterId)
            in
            { model | containers = newContainers, services = newServices, clusters = model.clusters |> Dict.remove clusterId }

        ChangeContainerName id value ->
            { model | containers = Dict.update id (Maybe.map (\container -> { container | name = value })) model.containers }

        ChangeDaemonName id value -> 
            { model | daemons = Dict.update id (Maybe.map (\daemon -> { daemon | name = value })) model.daemons}

        ChangeServiceName id value ->
            { model | services = Dict.update id (Maybe.map (\service -> { service | name = value })) model.services }
            
        ChangeClusterName id value ->
            { model | clusters = Dict.update id (Maybe.map (\cluster -> { cluster | name = value })) model.clusters }

view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ div []
            [ Util.viewColumnTitle "Configuration"
            , hr [] []
            ]
        , viewClusters model
        , hr [] []
        , ListGroup.custom
            [ simpleListItem "Filters" FeatherIcons.filter [ href "settings" ]
            , simpleListItem "Export JSON" FeatherIcons.share [ href "#" ]
            , simpleListItem "Load JSON" FeatherIcons.download [ href "#" ]
            ]
        ]


viewClusters : Model -> Html Msg
viewClusters model =
    div [ style "max-height" "60vh", style "overflow-y" "scroll" ]
        [ ListGroup.custom (List.concatMap (viewClusterItem model) (Dict.toList model.clusters))
        ]


viewClusterItem : Model -> ( Int, Cluster ) -> List (ListGroup.CustomItem Msg)
viewClusterItem model clusterTuple =
    let
        id =
            first clusterTuple

        cluster =
            second clusterTuple
    in
    List.concat
        [ [ ListGroup.anchor
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, class "cluster-item", href ("cluster/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.share2 |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], input [ type_ "text", class "editable-label", value cluster.name, onChange (ChangeClusterName id)] [] ]
                    , div []
                        [ span [] [ Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddService id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]

                        -- needed to prevent the onClick of the list item from firing, and rerouting us to a non-existant thingy
                        ]
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
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, href ("service/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], input [ type_ "text", class "editable-label", value service.name, onChange (ChangeServiceName id)] []]
                    , span [ class "text-muted", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteService id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                    ]
                ]
          ]
        , viewTaskItem id
        , viewContainers (getContainers id model.containers)
        ]


viewTaskItem : Int -> List (ListGroup.CustomItem Msg)
viewTaskItem id =
    [ ListGroup.anchor
        [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("task/" ++ String.fromInt id) ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Tasks" ]
            , span [] [ Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddContainer id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]
            ]
        ]
    ]


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


viewContainers : Containers -> List (ListGroup.CustomItem Msg)
viewContainers containers =
    List.map viewContainerItem (Dict.toList containers)


viewContainerItem : ( Int, Container ) -> ListGroup.CustomItem Msg
viewContainerItem containerTuple =
    let
        id =
            first containerTuple

        container =
            second containerTuple
    in
    ListGroup.anchor [ ListGroup.attrs [ href ("container/" ++ String.fromInt id), style "padding-left" "60px", style "border-left" ("4px solid " ++ container.color)] ]
        [ FeatherIcons.box |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml []
        , input [ type_ "text", class "editable-label", value container.name, onChange (ChangeContainerName id)] []
        , span [ class "ml-3 text-muted float-right", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteContainer id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        ]


simpleListItem : String -> FeatherIcons.Icon -> List (Html.Attribute Msg) -> ListGroup.CustomItem Msg
simpleListItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ icon |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text label ]
