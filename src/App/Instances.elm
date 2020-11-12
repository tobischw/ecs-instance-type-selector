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

type PreferredPricing
    = Reserved1Yr
    | Reserved3Yr
    | OnDemandPricing

type alias Model =
     { instances: Instances
     , filters: Filters
     , pricingType: PreferredPricing
     }
    
type alias Filters = 
    { os: List String
    , instanceType: List String
    , regions: List String
    }

type FilterType
    = OS
    | InstanceType
    | Region

type Msg
    = LoadInstances (Result Json.Decode.Error ApiDecoders.ProductsResponse)
    | SetFilters FilterType (List String)
    | SetPreferredPricing PreferredPricing

type alias Instances = List Instance
type alias Instance = 
     { sku: String                 -- The SKU (ID) of the EC2 instance
     , instanceType: String        -- The instance type (e.g. "m5ad.12xlarge")
     , location: String            -- This relates to the region, but for now, probably a good idea to store this (e.g. "EU (Ireland)")
     , operatingSystem: String     -- Probably a good idea to have this for future purposes
     , memory: Int                 -- The memory available, in MB. Make sure we convert to MB from whatever the API gives us.
     , vCPU: Int                   -- Number of vCPUs that this instance has available
     , prices: List BoxPricing
     }

type ContractLength
    = OneYear
    | ThreeYear

type ContractType
    = AllUpFront
    | NoUpFront
--    | PartialUpFront Float -- NOTE: Not dealing with this. Ignoring for l8r.

type BoxPricing
    = OnDemand Float -- OnDemand {HourlyCost}
    | Reserved ContractLength ContractType Float



defaultRegion : String 
defaultRegion =
    "us-east-1"

-- Setup

init : Model
init = {instances=[], filters={os=[], instanceType=[], regions=[]}, pricingType = Reserved1Yr}


defaultInstance : Instance
defaultInstance =
    { sku = ""
      , instanceType = ""
      , location = ""
      , operatingSystem = ""
      , memory = 0
      , vCPU = 0
      , prices = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveInstances (LoadInstances << decodeString ApiDecoders.productsResponseDecoder)


numInstancesBatched : Int
numInstancesBatched =
    100


maxInstancesTesting : Int 
maxInstancesTesting =
    1500


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
                    in
                    ({model | filters = { filters | os = filterData}}, Cmd.none)
                InstanceType ->
                    let
                        filters = model.filters
                    in
                    ({model | filters = { filters | instanceType = filterData}}, Cmd.none)
                Region ->
                    let
                        filters = model.filters
                    in
                    ({model | filters = { filters | regions = filterData}}, Cmd.none)
        SetPreferredPricing ptype -> 
            ({model | pricingType = ptype}, Cmd.none)
                

-- Mapping

mapToInstances : List ApiDecoders.PriceListing -> Instances
mapToInstances original =
    values <| List.map priceListingToInstance original 


findOptimalSuggestions: Model -> Int -> Int -> (Instance, Instances)
findOptimalSuggestions model vcpu memory =
   let 
        suggestions = model.instances 
            |> List.filter (isSuitableInstance vcpu memory)
            |> List.filter (isNotExludedInstance model.filters)
            |> List.filter (filterByPricing model.pricingType)
            |> List.sortBy .memory
            |> List.sortBy .vCPU
            --|> List.sortBy lowestPrice
             -- TODO: sort by lowest price
        top = List.head suggestions |> Maybe.withDefault defaultInstance
   in
        (top, removeAt 0 suggestions)

filterByPricing: PreferredPricing -> Instance -> Bool
filterByPricing preferred instance =
    List.any (pricingLambda preferred) instance.prices


pricingLambda: PreferredPricing -> BoxPricing -> Bool
pricingLambda preferred pricing =
    case preferred of
        Reserved1Yr -> 
            case pricing of
                Reserved length _ _ -> 
                    case length of
                        OneYear -> True
                        _ -> False
                _ -> False
        Reserved3Yr ->
            case pricing of
                Reserved length _ _ -> 
                    case length of
                        ThreeYear -> True
                        _ -> False
                _ -> False
        OnDemandPricing ->
            case pricing of
                OnDemand _ -> True
                _ -> False    

--lowestPrice: Instance -> Instance -> Instances -> Instances
--lowestPrice instance compare instances =
 --   []



isNotExludedInstance: Filters -> Instance -> Bool 
isNotExludedInstance filters instance =
    let
        osExcluded = List.member instance.operatingSystem filters.os
        typeExcluded = isNotExcludedInstanceType filters instance -- TODO: Fix and actually make this work

        regionIncluded = isIncludedRegion filters instance
    in
        not osExcluded && not typeExcluded && regionIncluded

isNotExcludedInstanceType: Filters -> Instance -> Bool 
isNotExcludedInstanceType filters instance =
    let
        itype = instance.instanceType
    in
        List.any (\item -> String.startsWith item itype) filters.instanceType


isIncludedRegion: Filters -> Instance -> Bool 
isIncludedRegion filters instance =
    let
        region = instance.location
    in
        List.any (\item -> String.startsWith item region) filters.regions
    
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
        onDemandPrices = List.concatMap termToPrices original.terms.onDemand
        reservedPrices = List.concatMap termToPrices original.terms.reserved
        pricingList = onDemandPrices ++ reservedPrices
    in 
        if memory > 0 && vCPU >= 0 && (areValidPrices onDemandPrices || areValidPrices reservedPrices) then
            Just (Instance sku instanceType location operatingSystem memory vCPU pricingList)
        else
            Nothing

areValidPrices : List BoxPricing -> Bool 
areValidPrices prices =
    if List.length prices > 0 then
        List.all isValidPrice prices
    else
        False


-- We should probably filter these out when we do the decoding, not here
isValidPrice : BoxPricing -> Bool 
isValidPrice boxPricing =
    case boxPricing of
        OnDemand price ->
            price > 0
        Reserved _ _ price ->
            price > 0


termToPrices : ApiDecoders.Term -> List BoxPricing
termToPrices term =
    let
        termAttributes = term.termAttributes
        priceDimensions = term.priceDimensions
    in
        List.map (termToBoxPricing termAttributes) priceDimensions


termToBoxPricing : ApiDecoders.TermAttributes -> ApiDecoders.PriceDimension -> BoxPricing
termToBoxPricing termAttributes dimension =
    let 
        unitPrice = String.toFloat dimension.pricePerUnit.usd |> Maybe.withDefault 0
    in
        -- Ideally, these should be Maybe's, not just be empty strings
        if String.isEmpty termAttributes.leaseContractLength ||
           String.isEmpty termAttributes.purchaseOption then
            OnDemand unitPrice
        else
            let
                contractLength = contractLengthStringConverter termAttributes.leaseContractLength

                purchaseOption = contractTypeStringConverter termAttributes.purchaseOption

                length = Maybe.withDefault OneYear contractLength
                yearScalar = case length of
                    OneYear -> 1
                    ThreeYear -> 3
                    
            in
            Reserved length (Maybe.withDefault NoUpFront purchaseOption) ((unitPrice / (365 * yearScalar)) / 24)


contractLengthStringConverter: String -> Maybe ContractLength
contractLengthStringConverter value =
    case value of
        "1yr" -> Just OneYear
        "3yr" -> Just ThreeYear
        _ -> Nothing

contractTypeStringConverter: String -> Maybe ContractType
contractTypeStringConverter value =
    case value of 
        "All Upfront" -> Just AllUpFront
        "No Upfront" -> Just NoUpFront
        _ -> Nothing

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
