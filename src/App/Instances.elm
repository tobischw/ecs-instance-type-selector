port module App.Instances exposing (..)

import App.ApiDecoders as ApiDecoders
import Json.Decode exposing (Error(..), decodeString)
import Html exposing (input)
import Array

---- PORTS ----

port requestInstances : ( String, Int ) -> Cmd msg


port receiveInstances : (String -> msg) -> Sub msg


-- Model

type alias Model =
     Instances
    

type Msg 
    = LoadInstances (Result Json.Decode.Error ApiDecoders.ProductsResponse)

type alias Instances = List Instance
type alias Instance = 
     { sku: String                 -- The SKU (ID) of the EC2 instance
     , instanceType: String        -- The instance type (e.g. "m5ad.12xlarge")
     , location: String            -- This relates to the region, but for now, probably a good idea to store this (e.g. "EU (Ireland)")
     , operatingSystem: String     -- Probably a good idea to have this for future purposes
     , memory: Int                 -- The memory available, in MB. Make sure we convert to MB from whatever the API gives us.
     , vCPU: Int                   -- Number of vCPUs that this instance has available
     --, prices: List PricingInfo
     }

type Price         -- Filter out any non-USD data
     = Upfront Float
     | Hourly Float

type PricingType 
    = OnDemand 
    | Reserved

type alias PricingInfo = 
     { pricingType: PricingType
     , offerTermCode: String    -- The code we need to show for pricing
     , price: Price
     }
 

-- Setup

init : Model
init = []


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveInstances (LoadInstances << decodeString ApiDecoders.productsResponseDecoder)


numInstancesBatched : Int
numInstancesBatched =
    90


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadInstances (Ok response) ->
            let
                simplified = mapToInstances response.priceList
            in
            ( model ++ simplified, requestInstances ( response.nextToken, numInstancesBatched ) )

        LoadInstances (Err err) ->
            ( model, Cmd.none )


-- Mapping

mapToInstances : List ApiDecoders.PriceListing -> Instances
mapToInstances original =
    List.map priceListingToInstance original 


priceListingToInstance : ApiDecoders.PriceListing -> Instance
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
    in 
        (Instance sku instanceType location operatingSystem memory vCPU)


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