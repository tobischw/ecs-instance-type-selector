port module App.Instances exposing (..)

import App.ApiDecoders as ApiDecoders
import Json.Decode exposing (Error(..), decodeString)
import Array
import Bootstrap.Utilities.Border exposing (rounded)
import Maybe.Extra exposing (..)
import List.Extra exposing (..)
import Html exposing (i)

---- PORTS ----

port requestInstances : ( String, String, Int ) -> Cmd msg


port receiveInstances : (String -> msg) -> Sub msg


-- Model

type alias Model =
     { instances: Instances
     , filters: Filters
     , test: String
     }
    
type alias Filters = 
    { os: List String
    , instanceType: List String
    }

type FilterType
    = OS
    | InstanceType

type Msg 
    = LoadInstances (Result Json.Decode.Error ApiDecoders.ProductsResponse)
    | SetFilters FilterType (List String)

type alias Instances = List Instance
type alias Instance = 
     { sku: String                 -- The SKU (ID) of the EC2 instance
     , instanceType: String        -- The instance type (e.g. "m5ad.12xlarge")
     , location: String            -- This relates to the region, but for now, probably a good idea to store this (e.g. "EU (Ireland)")
     , operatingSystem: String     -- Probably a good idea to have this for future purposes
     , memory: Int                 -- The memory available, in MB. Make sure we convert to MB from whatever the API gives us.
     , vCPU: Int                   -- Number of vCPUs that this instance has available
     , onDemandPrices: PriceTerm 
     , reservedPrices: PriceTerm
     }

type Price         -- Filter out any non-USD data
     = Upfront Float
     | Hourly Float


type PriceTerm
    = OnDemand (List Price)
    | Reserved (List Price)

defaultRegion : String 
defaultRegion =
    "us-east-1"

-- Setup

init : Model
init = {instances=[], test = "", filters={os=[], instanceType=[]}}


defaultInstance : Instance
defaultInstance =
    { sku = ""
      , instanceType = ""
      , location = ""
      , operatingSystem = ""
      , memory = 0
      , vCPU = 0
      , onDemandPrices = (OnDemand [])
      , reservedPrices = (Reserved [])
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveInstances (LoadInstances << decodeString ApiDecoders.productsResponseDecoder)


numInstancesBatched : Int
numInstancesBatched =
    100


maxInstancesTesting : Int 
maxInstancesTesting =
    1000


--updateWithFilters : 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadInstances (Ok response) ->
            let
                simplified = mapToInstances response.priceList
                totalCount = List.length model.instances

                region = defaultRegion
                nextCommand =
                -- Ensure we do not exceed our max limit of instances
                    if totalCount < maxInstancesTesting - numInstancesBatched then
                        requestInstances ( region, response.nextToken, numInstancesBatched )
                    else
                        Cmd.none
            in
            ( {model | instances = model.instances ++ simplified}, nextCommand )

        LoadInstances (Err err) ->
            let
                _ = Debug.log "Instance Load Error" err
            in
            ( model, Cmd.none )

        SetFilters filterType filterData ->
            case filterType of
                OS -> 
                    let
                        filters = model.filters
                        _ = Debug.log "Filters" filters
                        _ = Debug.log "FilterData" filterData
                    in
                    ({model | filters = { filters | os = filterData}}, Cmd.none)
                _ ->
                    (model, Cmd.none)
                

-- Mapping

mapToInstances : List ApiDecoders.PriceListing -> Instances
mapToInstances original =
    values <| List.map priceListingToInstance original 


findOptimalSuggestions: Filters -> Instances -> Int -> Int -> Int -> (Instance, Instances)
findOptimalSuggestions filters instances vcpu memory numSuggestions =
   let 
        _ = Debug.log "Filters" filters
        suggestions = instances 
            |> List.filter (isSuitableInstance vcpu memory)
            |> List.filter (isNotExludedInstance filters)
            |> List.sortBy .memory
            |> List.sortBy .vCPU
            |> List.take numSuggestions
        top = List.head suggestions |> Maybe.withDefault defaultInstance
   in
        (top, removeAt 0 suggestions)



isNotExludedInstance: Filters -> Instance -> Bool 
isNotExludedInstance filters instance =
    let
        osExcluded = List.member instance.operatingSystem filters.os
        typeExcluded = List.member instance.instanceType filters.instanceType -- TODO: Fix and actually make this work
    in
        not osExcluded && not typeExcluded

isSuitableInstance : Int -> Int -> Instance -> Bool
isSuitableInstance vcpu memory instance =
    let
        share = round <| toFloat vcpu / 1024
    in
    instance.memory >= memory && instance.vCPU >= share


priceListingToInstance : ApiDecoders.PriceListing -> Maybe Instance
priceListingToInstance original =
    let
        product = original.product
        attributes = product.attributes
        sku = product.sku
        instanceType = attributes.instanceType
        location = attributes.location
        operatingSystem = attributes.operatingSystem
        memory = attributes.memory |> convertMemoryStringToMiB
        vCPU = attributes.vCPU |> String.toInt |> Maybe.withDefault 0
        onDemandDimensions = termsToPriceDimensions original.terms.onDemand
        onDemandPrices = List.map priceDimensionToPriceInfo onDemandDimensions
        reservedDimensions = termsToPriceDimensions original.terms.reserved
        reservedPrices = List.map priceDimensionToPriceInfo reservedDimensions
    in 
        if memory > 0 && vCPU >= 0 && (areValidPrices onDemandPrices || areValidPrices reservedPrices) then
            Just (Instance sku instanceType location operatingSystem memory vCPU (OnDemand onDemandPrices) (Reserved reservedPrices))
        else
            Nothing


areValidPrices : List Price -> Bool 
areValidPrices prices =
    if List.length prices > 0 then
        List.all isValidPrice prices
    else
        False

-- We should probably filter these out when we do the decoding, not here
isValidPrice : Price -> Bool 
isValidPrice price =
    case price of
        Upfront value ->
            value > 0
        Hourly value ->
            value > 0


termsToPriceDimensions : List ApiDecoders.Term -> List ApiDecoders.PriceDimension 
termsToPriceDimensions terms =
    List.concatMap (\term -> term.priceDimensions) terms


priceDimensionToPriceInfo : ApiDecoders.PriceDimension -> Price
priceDimensionToPriceInfo dimension =
    let 
        unitPrice = String.toFloat dimension.pricePerUnit.usd |> Maybe.withDefault 0
    in
        if dimension.unit == "Hrs" then
            Hourly unitPrice
        else
            Upfront unitPrice


convertMemoryStringToMiB : String -> Int
convertMemoryStringToMiB input =
    let
        result = String.split " " input |> Array.fromList
        value = Array.get 0 result |> Maybe.withDefault "" |> String.toInt |> Maybe.withDefault 0
        unit = Array.get 1 result |> Maybe.withDefault ""
    in
        case unit of
           "MiB" -> value
           "GiB" -> value * 1024
           "TiB" -> value * 1024 * 1024
           _ -> 0
