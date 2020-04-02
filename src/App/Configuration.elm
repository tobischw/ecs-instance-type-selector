module App.Configuration exposing (Cluster, Container, Model, Msg(..), Service, init, update, view)

import App.Constants exposing (RegionRecord, allRegions)
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


init : Model
init =
    { clusters = Dict.fromList [ ( 0, Cluster "Cluster 1" ) ]
    , services = Dict.fromList [ ( 0, Service "Service 1" 0 0 (Multiselect.initModel [] "A") 1 1 ), ( 1, Service "Service 2" 0 0 (Multiselect.initModel [] "A") 1 1 ) ]
    , containers = Dict.fromList [ ( 0, Container "Container A" 0 20 20 20 False 20 False ), ( 1, Container "Container B" 0 20 20 20 False 20 False ) ]
    , autoIncrement = 2 -- Set this to 0 once we get rid of sample data
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
    , autoIncrement : Int
    }


type Msg
    = AddCluster
    | AddService Int
    | AddContainer Int
    | DeleteContainer Int
    | DeleteService Int
    | DeleteCluster Int


type alias Cluster =
    { name : String
    }


type alias Service =
    { name : String
    , clusterId : Int
    , scalingTarget : Int
    , regions : Multiselect.Model
    , minTasks : Int
    , maxTasks : Int
    }


type alias Container =
    { name : String
    , serviceId : Int
    , cpuShare : Int
    , memory : Int
    , ioops : Int
    , useEBS : Bool
    , bandwidth : Int
    , displayExtraMemory: Bool
    }


generateId : Model -> Int
generateId model =
    model.autoIncrement + 1


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCluster ->
            { model | clusters = model.clusters |> Dict.insert model.autoIncrement (Cluster "Cluster"), autoIncrement = generateId model }

        AddService clusterId ->
            { model | services = model.services |> Dict.insert model.autoIncrement (Service "Service" clusterId 0 (Multiselect.initModel [] "A") 1 1), autoIncrement = generateId model }

        AddContainer serviceId ->
            { model | containers = model.containers |> Dict.insert model.autoIncrement (Container "Container" serviceId 128 2048 128 True 1048 False), autoIncrement = generateId model }

        DeleteContainer containerId ->
            { model | containers = model.containers |> Dict.remove containerId }

        DeleteService serviceId ->
            let
                newModel =
                    { model | containers = model.containers |> Dict.Extra.removeWhen (\_ container -> container.serviceId == serviceId) }
            in
            { newModel | services = model.services |> Dict.remove serviceId }

        DeleteCluster clusterId ->
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


view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1"][
        div [] 
            [ Util.viewColumnTitle "Configuration"
            , hr [][]
        ],
        div [] [
            Button.button 
            [ Button.outlineSuccess
            , Button.onClick AddCluster
            , Button.block
            , Button.attrs [ class "mb-2" ]
            ] [ FeatherIcons.plus |> FeatherIcons.toHtml [], text "Add Cluster" ]
            , hr [][] 
        ],
        viewClusters model
        , hr [][]
        , ListGroup.custom 
        [ simpleListItem "Global Settings" FeatherIcons.settings [ href "../../settings" ]
        , simpleListItem "Export JSON" FeatherIcons.share [ href "#" ]
        , simpleListItem "Load JSON" FeatherIcons.download [ href "#"]
        ]
    ]

viewClusters : Model -> Html Msg
viewClusters model =
    div [style "max-height" "60vh", style "overflow-y" "scroll"] [
        ListGroup.custom(List.concatMap (viewClusterItem model) (Dict.toList model.clusters))
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
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, class "cluster-item", href ("/cluster/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.share2 |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text cluster.name ]
                    , div []
                        [ span [] [ Button.button [ Button.outlineSecondary, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddService id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]
                        , span [ class "ml-3 text-danger", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteCluster id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]

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
                [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, href ("/service/" ++ String.fromInt id) ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ span [ class "pt-1" ] [ FeatherIcons.server |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text service.name ]
                    , span [ class "text-danger", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteService id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                    ]
                ]
          ]
        , viewTaskItem id
        , viewContainers (getContainers id model.containers)
        ]


viewTaskItem : Int -> List (ListGroup.CustomItem Msg)
viewTaskItem id =
    [ ListGroup.anchor
        [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, style "padding-left" "40px", href ("/task/" ++ String.fromInt id) ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ span [ class "pt-1" ] [ FeatherIcons.clipboard |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text "Tasks" ]
            , span [] [ Button.button [ Button.outlineSuccess, Button.small, Button.attrs [ Html.Events.Extra.onClickPreventDefaultAndStopPropagation (AddContainer id) ] ] [ FeatherIcons.plus |> FeatherIcons.withSize 16 |> FeatherIcons.withClass "empty-button" |> FeatherIcons.toHtml [], text "" ] ]
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
    ListGroup.anchor [ ListGroup.attrs [ href ("/container/" ++ String.fromInt id), style "padding-left" "60px" ] ]
        [ FeatherIcons.box |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml []
        , text container.name
        , span [ class "ml-3 text-danger float-right", Html.Events.Extra.onClickPreventDefaultAndStopPropagation (DeleteContainer id) ] [ FeatherIcons.trash2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        ]


simpleListItem : String -> FeatherIcons.Icon -> List (Html.Attribute Msg) -> ListGroup.CustomItem Msg
simpleListItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ icon |> FeatherIcons.withSize 19 |> FeatherIcons.toHtml [], text label ]
