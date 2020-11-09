module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances, isSuitableInstance)
import App.Visualization exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h3)
import Html.Attributes exposing (class, style)
import FormatNumber.Locales exposing (usLocale, Locale, Decimals(..))
import FormatNumber exposing (format)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Svg exposing (Svg, g, a, rect, svg, line, text_)
import Svg.Attributes exposing (alignmentBaseline, xlinkHref, fontSize, fill, height, stroke, strokeWidth, strokeDasharray, textAnchor, transform, width, x, y, x1, x2, y1, y2)
import App.Configuration exposing (Daemons)
import List.Extra exposing (mapAccuml)


type alias Model = 
    {
     configuration : Configuration.Model
    , instances: Instances.Model
    }


type alias ContainerData =
    { name : String
    , color: String
    }


sharesLocale : Locale
sharesLocale =
    { usLocale
        | decimals = Exact 2
        , negativePrefix = "("
        , negativeSuffix = ")"
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
        containers = model.configuration.containers
        boxes = convertToBoxes services containers

        visualization = prepareVisualization boxes 
        share = round <| visualization.width / 1024
        memory = round <| visualization.height
        showSuggestions = (Dict.isEmpty model.configuration.containers == False)

        (topSuggestion, remainingSuggestions) = 
            if showSuggestions then
                Instances.findOptimalSuggestions model.instances.filters model.instances.instances share memory
            else
               (Instances.defaultInstance, []) 

        topRemainingSuggestions = List.take 5 remainingSuggestions

        topWidth = (toFloat topSuggestion.vCPU * 1024)
        topHeight = (toFloat topSuggestion.memory)
    in
    div []
        [ 
          if showSuggestions then 
          div [] 
          [ 
             h3 [] [ text ("Total: $" ++ format sharesLocale (getPriceForTopSuggestion model topSuggestion) ++ "/mo")]
            , span [] [ text "We determined that ", strong [] [ text "a single instance" ], text " is a good fit:"]
            , viewInstanceListing topSuggestion
            , hr [] []
            , text ("Ideal CPU share: " ++ String.fromInt share)
            , br [] []
            , text ("Ideal memory: " ++ Util.formatMegabytes memory) 
            , br [] []
            , text ("Results matching requirements: " ++ String.fromInt (List.length remainingSuggestions))
            , hr [] []
            , viewVisualization visualization (topWidth, topHeight)
        ]
        else
            span [] [ text "No results or suggestions available yet."]
        ]


getPriceForTopSuggestion: Model -> Instance -> Float 
getPriceForTopSuggestion model topSuggestion =
    let
        pricesTmp = 
            case topSuggestion.onDemandPrices of 
                Instances.OnDemand lst ->
                        lst
                _ -> 
                        []
        prices = List.map mapPrices pricesTmp
        output = List.maximum prices |> Maybe.withDefault 0
        
    in
        output * 30 * 24

mapPrices: Instances.Price -> Float 
mapPrices price =
    case price of
       Instances.Hourly p _ -> p
       _ -> 0


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
            , Block.text [] [ text instance.location ]
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
        Instances.Upfront value rateCode ->
            li [] [ text <| "$" ++ String.fromFloat value ++ " upfront ", span [ class "subtle"] [ text rateCode] ]

        Instances.Hourly value rateCode ->
            li [] [ text <| "$" ++ String.fromFloat value ++ "/hr ", span [ class "subtle"] [ text rateCode] ]


convertToBoxes : Configuration.Services -> Configuration.Containers -> List Box 
convertToBoxes services containers =
    let
        containersList = Dict.toList containers

        initialBoxes = List.concatMap (convertToRepeatedBox services) containersList
    in
    initialBoxes


convertToRepeatedBox : Configuration.Services -> (Int, Configuration.Container) -> List Box
convertToRepeatedBox services (id, container) =
    let
        service = Dict.get container.serviceId services |> Maybe.withDefault (Configuration.Service "" 0 0 App.Configuration.ByCPUShares 0 0 0)
        cpuShare = (toFloat container.cpuShare)
        memory = (toFloat container.memory)
        sortValue =
            case service.packingStrategy of
                App.Configuration.ByCPUShares -> cpuShare
                App.Configuration.ByMemory -> memory
    in
    List.repeat service.nominalTasks (Box id container.name service.name container.color 0 0 cpuShare memory sortValue)
