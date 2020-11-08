module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances, isSuitableInstance)
import App.Visualization exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h2)
import Html.Attributes exposing (class, style)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Svg exposing (Svg, g, a, rect, svg, line, text_)
import Svg.Attributes exposing (alignmentBaseline, xlinkHref, fontSize, fill, height, stroke, strokeWidth, strokeDasharray, textAnchor, transform, width, x, y, x1, x2, y1, y2)
import App.Configuration exposing (Daemons)


type alias Model = 
    {
     configuration : Configuration.Model
    , instances: Instances.Model
    }


type alias ContainerData =
    { name : String
    , color: String
    }




view : Model -> Html msg
view model =
    div [ class "pt-1", class "px-3" ]
        [ Util.viewColumnTitle
            "Results"
        , hr [] []
        , viewResultsForService model
        ]


viewResultsForService : Model -> Html msg
viewResultsForService model =
    let
        services = model.configuration.services
        containers = Dict.toList model.configuration.containers
        boxes = List.map (convertToBox services) containers
        visualization = prepareVisualization boxes 
        share = round <| visualization.width / 1024
        memory = round <| visualization.height
        showSuggestions = (Dict.isEmpty model.configuration.containers == False)

        (topSuggestion, remainingSuggestions) = 
            if showSuggestions then
                Instances.findOptimalSuggestions model.instances.filters model.instances.instances share memory 5
            else
               (Instances.defaultInstance, []) 

        topWidth = (toFloat topSuggestion.vCPU * 1024)
        topHeight = (toFloat topSuggestion.memory)
    in
    div []
        [ 
          if showSuggestions then 
          div [] [  
          viewVisualization visualization (topWidth, topHeight)
        , hr [] []
        , text ("Ideal CPU share: " ++ String.fromInt share)
        , br [] []
        , text ("Ideal memory: " ++ Util.formatMegabytes memory) 
        , hr [] []
        , h2 [] [ text "Total: $0/mo"]
        , strong [] [ text "Top Suggestion:"]
        , viewInstanceListing topSuggestion
        , hr [] []
        , strong [] [ text "Other Suggestions:"]
        , viewSuggestions remainingSuggestions 
        , hr [] [] ]
        else
            span [] [ text "No results or suggestions available yet."]
        ]


viewSuggestions : Instances -> Html msg 
viewSuggestions instances = 
    div []
        (List.map viewInstanceListing instances)


viewInstanceListing : Instance -> Html msg
viewInstanceListing instance =
    div [ style "margin-top" "10px"] [
        Card.config []
        |> Card.block []
            [ Block.text [] [ strong [] [ text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> Util.formatMegabytes) ++ " (" ++ instance.operatingSystem ++")") ] ] 
            , Block.custom <| div [] [ viewPriceList instance.onDemandPrices ]
            , Block.custom <| div [] [ viewPriceList instance.reservedPrices ]
            ]
        |> Card.view
    ]


viewPriceList : Instances.PriceTerm -> Html msg 
viewPriceList priceTerm =
    case priceTerm of
        Instances.OnDemand prices ->
            if List.length prices > 0 then
                div [] [ text "OnDemand:", ul [] (List.map viewPrice prices) ]
            else
                span [] []

        Instances.Reserved prices ->
            if List.length prices > 0 then
                div [] [ text "Reserved:", ul [] (List.map viewPrice prices) ]
            else
                span [] []


viewPrice :  Instances.Price -> Html msg
viewPrice price =
    case price of
        Instances.Upfront value ->
            li [] [ text <| "$" ++ String.fromFloat value ++ " upfront" ]

        Instances.Hourly value ->
            li [] [ text <| "$" ++ String.fromFloat value ++ "/hr" ]


convertToBox : Configuration.Services -> (Int, Configuration.Container) -> Box
convertToBox services (id, container) =
    let
        service = Dict.get container.serviceId services |> Maybe.withDefault (Configuration.Service "" 0 0 App.Configuration.ByCPUShares 0 0 0)
        cpuShare = (toFloat container.cpuShare)
        memory = (toFloat container.memory)
        sortValue =
            case service.packingStrategy of
                App.Configuration.ByCPUShares -> cpuShare
                App.Configuration.ByMemory -> memory
    in
    (Box id container.name container.color 0 0 cpuShare memory sortValue)
