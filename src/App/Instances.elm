port module App.Instances exposing (..)

import App.ApiDecoders as ApiDecoders
import Json.Decode exposing (Error(..), decodeString)


---- PORTS ----

port requestInstances : ( String, Int ) -> Cmd msg


port receiveInstances : (String -> msg) -> Sub msg


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
            ( model ++ response.priceList, requestInstances ( response.nextToken, numInstancesBatched ) )

        LoadInstances (Err err) ->
            ( model, Cmd.none )


-- Model

type alias Model =
     List ApiDecoders.PriceListing
    

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
     , prices: List PricingInfo
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

