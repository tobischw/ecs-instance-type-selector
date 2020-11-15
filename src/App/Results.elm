module App.Results exposing (..)

import App.Configuration as Configuration
import App.Daemon as Daemon exposing (sumDaemonResources, daemonsForContainer)
import App.Util as Util
import App.Instances as Instances exposing (Instance, Instances, isSuitableInstance)
import App.Visualization exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, br, canvas, div, hr, p, small, span, strong, text, ul, li, h3, h4)
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

hourlyLocale : Locale
hourlyLocale =
    { usLocale
        | decimals = Exact 8
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
                Instances.findOptimalSuggestions model.instances share memory
            else
               (Instances.defaultInstance, []) 

        topRemainingSuggestions = List.take 5 remainingSuggestions

        topWidth = (toFloat topSuggestion.vCPU * 1024)
        topHeight = (toFloat topSuggestion.memory)

        (monthly, yearly) = getPriceForTopSuggestion model topSuggestion
    in
    div []
        [ 
          if showSuggestions then 
          div [] 
          [ 
             h3 [] [ text ("Total: $" ++ (format sharesLocale monthly) ++ "/mo")]
            , strong [] [ text ("$" ++ format sharesLocale yearly ++ "/yr")]
            , br [] []
            , span [] [ text "We determined that ", strong [] [ text "a single instance" ], text " is a good fit:"]
            , viewInstanceListing model.instances.pricingType topSuggestion
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


getPriceForTopSuggestion: Model -> Instance -> (Float, Float)
getPriceForTopSuggestion model topSuggestion =
    let
        prices = List.map mapPrices topSuggestion.prices
        output = List.maximum prices |> Maybe.withDefault 0
        
        monthly = output * 30 * 24
        yearly = monthly * 12
    in
        (monthly, yearly)


mapPrices : Instances.BoxPricing -> Float
mapPrices price =
    case price of
        Instances.OnDemand _ value -> value
        Instances.Reserved _ _ _ value -> value


viewInstanceListing : Instances.PreferredPricing -> Instance -> Html msg
viewInstanceListing prefPrice instance =
    div [ style "margin-top" "10px"] [
        Card.config []
        |> Card.block []
            [ Block.text [] [ h4 [] [ text (instance.instanceType ++ ", " ++ (instance.vCPU |> String.fromInt) ++ "vCPUs, " ++ (instance.memory |> Util.formatMegabytes) ++ " (" ++ instance.operatingSystem ++")") ] ] 
            , Block.text [] [ text instance.location ]
            , Block.custom <| viewPriceList prefPrice instance.prices
            ]
        |> Card.view
    ]


viewPriceList : Instances.PreferredPricing -> List Instances.BoxPricing -> Html msg 
viewPriceList prefPrice prices =
    let 
        newPrices = List.filter (Instances.pricingLambda prefPrice) prices
    in
        ul [class "priceList"] (List.map viewPrice newPrices)


viewPrice : Instances.BoxPricing -> Html msg
viewPrice price =
    case price of
        Instances.OnDemand rateCode hourlyCost ->
            li [] [ text <| "$" ++ String.fromFloat hourlyCost ++ "/hr ", span [ class "subtle"] [ text (" " ++ rateCode) ] ]

        Instances.Reserved rateCode contractLength contractType hourlyCost ->
            case contractType of
                Instances.AllUpFront -> viewReservedAllUpFront rateCode hourlyCost contractLength
                Instances.NoUpFront -> viewReservedAllNoUpFront rateCode hourlyCost contractLength


viewReservedAllUpFront: String -> Float -> Instances.ContractLength -> Html msg
viewReservedAllUpFront rateCode hourlyCost contractLength =
    let 
        hourlyStr = format hourlyLocale hourlyCost
        upfrontCost = hourlyCost * (365 * scalar * 24)
        upfrontStr = format sharesLocale upfrontCost
        scalar = case contractLength of
            Instances.OneYear -> 1
            Instances.ThreeYear -> 3
    in
        li [] [ text ("$" ++ hourlyStr ++ "/hr (All upfront)")
              , span [class "subtle"] [ text (" " ++ rateCode )]
              ,  ul [] [
                  li [] [
                      text ("$" ++ upfrontStr ++ " upfront with " ++ String.fromInt scalar ++ " year contract")
                  ]
                ]
        ]

viewReservedAllNoUpFront: String -> Float -> Instances.ContractLength -> Html msg
viewReservedAllNoUpFront rateCode hourlyCost contractLength = 
    let 
        hourlyStr = format hourlyLocale hourlyCost
        scalar = case contractLength of
            Instances.OneYear -> 1
            Instances.ThreeYear -> 3
    in
        li [] [ text ("$" ++ hourlyStr ++ "/hr")
              , span [class "subtle"] [ text (" " ++ rateCode) ]
              ,  ul [] [
                  li [] [
                      text ("With " ++ String.fromInt scalar ++ " year contract")
                  ]
                ]
        ]


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
