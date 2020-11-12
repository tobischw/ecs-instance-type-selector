module App.ApiDecoders exposing (Attributes, PriceListing, Product, ProductsResponse, TermAttributes, productsResponseDecoder, PriceDimension, PricePerUnit, Term)

import Json.Decode exposing (Decoder, map, list, string, succeed, keyValuePairs, fail, field)
import Json.Decode.Pipeline exposing (hardcoded, required, optional)


---- MODEL ----


type alias ProductsResponse =
    { formatVersion : String
    , priceList : List PriceListing -- Since the repsonse is weird, the ProductList is necessary as an intermediary
    , nextToken : String
    }


type alias PriceListing =
    { product : Product
    , terms: Terms
    }

type alias PriceDimension =
    { name: String
    , unit: String
    , description: String
    , rateCode: String
    , pricePerUnit: PricePerUnit
    }

type alias PricePerUnit =
    { usd: String
    }

type alias Product =
    { sku : String
    , attributes : Attributes
    }

type alias Attributes =
    { memory : String
    , instanceType : String
    , location : String
    , operatingSystem : String
    , vCPU : String
    }

type alias Terms = 
    { onDemand: List Term
    , reserved: List Term
    }

    
type alias Term =
    { name: String,
      priceDimensions: List PriceDimension,
      offerTermCode: String,
      termAttributes: TermAttributes
    }

type alias TermAttributes =
    { leaseContractLength: String
    , purchaseOption: String
    }
    
-- type LeaseContractLength
--     = OneYear
--     | ThreeYear
    
-- type PurchaseOption
--     = NoUpfront
--     | AllUpfront
--     | PartialUpfront

---- DECODERS ----

productsResponseDecoder : Decoder ProductsResponse
productsResponseDecoder =
    succeed ProductsResponse
        |> required "FormatVersion" string
        |> required "PriceList" (list priceListingDecoder)
        |> required "NextToken" string


priceListingDecoder : Decoder PriceListing
priceListingDecoder =
    succeed PriceListing
        |> required "product" productDecoder
        |> required "terms" termsDecoder


productDecoder : Decoder Product
productDecoder =
    succeed Product
        |> required "sku" string
        |> required "attributes" attributesDecoder


attributesDecoder : Decoder Attributes
attributesDecoder =
    succeed Attributes
        |> optional "memory" string ""
        |> optional "instanceType" string "Unknown"
        |> optional "location" string "Unknown"
        |> optional "operatingSystem" string "Unknown"
        |> optional "vcpu" string ""


termsDecoder : Decoder Terms
termsDecoder =
    succeed Terms
        |> optional "OnDemand" unknownTermsKeyDecoder []
        |> optional "Reserved" unknownTermsKeyDecoder []


unknownTermsKeyDecoder : Decoder (List Term)
unknownTermsKeyDecoder =
    keyValuePairs termDecoder
        |> map buildTerms


buildTerms : List ( String, Term ) -> (List Term)
buildTerms terms =
    List.map (\(name, term) -> { term | name = name }) terms


termDecoder : Decoder Term
termDecoder =
    succeed Term
        |> hardcoded ""
        |> required "priceDimensions" unknownPriceDimensionsKeyDecoder
        |> required "offerTermCode" string
        |> optional "termAttributes" termAttributesDecoder (TermAttributes "" "")


termAttributesDecoder : Decoder TermAttributes
termAttributesDecoder =
    succeed TermAttributes
        |> optional "LeaseContractLength" string ""
        |> optional "PurchaseOption" string ""


-- Price Dimensions

buildPriceDimensions : List ( String, PriceDimension ) -> List PriceDimension
buildPriceDimensions priceDimensions =
    List.map (\(name, priceDimension) -> { priceDimension | name = name }) priceDimensions


unknownPriceDimensionsKeyDecoder : Decoder (List PriceDimension)
unknownPriceDimensionsKeyDecoder =
   keyValuePairs priceDimensionDecoder
        |> map buildPriceDimensions


priceDimensionDecoder : Decoder PriceDimension
priceDimensionDecoder =
    succeed PriceDimension
        |> hardcoded ""
        |> required "unit" string
        |> required "description" string
        |> required "rateCode" string
        |> required "pricePerUnit" pricePerUnitDecoder


pricePerUnitDecoder : Decoder PricePerUnit
pricePerUnitDecoder = 
    succeed PricePerUnit
        |> optional "USD" string "N/A"