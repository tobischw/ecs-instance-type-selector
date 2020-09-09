module App.ApiDecoders exposing (Attributes, PriceListing, Product, ProductsResponse, productsResponseDecoder, PriceDimension, PricePerUnit)

import Json.Decode exposing (Decoder, map, list, string, succeed, keyValuePairs)
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

type alias PriceDimensions =
    { priceDimensions: List PriceDimension
    }

type alias PriceDimension =
    { name: String
    , unit: String
    , endRange: String
    , description: String
    , rateCode: String
    , beginRange: String
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
    , operatingSystem: String
    , physicalProcessor : String
    }

type alias Terms = 
    { onDemand: TermsCollection
   -- , reserved: List Term
    }

type alias TermsCollection = 
    { terms: List Term
    }

    
type alias Term =
    { name: String,
      priceDimensions: PriceDimensions,
      offerTermCode: String 
    }

---- DECODERS ----

productsResponseDecoder : Decoder ProductsResponse
productsResponseDecoder =
    succeed ProductsResponse
        |> required "FormatVersion" string
        |> required "PriceList" (list priceListingDecoder)
        |> required "NextToken" string


productDecoder : Decoder Product
productDecoder =
    succeed Product
        |> required "sku" string
        |> required "attributes" attributesDecoder


attributesDecoder : Decoder Attributes
attributesDecoder =
    succeed Attributes
        |> required "memory" string
        |> required "instanceType" string
        |> required "operatingSystem" string
        |> required "vcpu" string


termsDecoder : Decoder Terms
termsDecoder =
    succeed Terms
        |> optional "OnDemand" unknownTermsKeyDecoder (TermsCollection [])


unknownTermsKeyDecoder : Decoder TermsCollection
unknownTermsKeyDecoder =
    keyValuePairs termDecoder
        |> map buildTerms


buildTerms : List ( String, Term ) -> TermsCollection
buildTerms terms =
    TermsCollection (List.map (\(name, term) -> { term | name = name }) terms)


termDecoder : Decoder Term
termDecoder =
    succeed Term
        |> hardcoded ""
        |> required "priceDimensions" unknownPriceDimensionsKeyDecoder
        |> required "offerTermCode" string


-- Price Dimensions

buildPriceDimensions : List ( String, PriceDimension ) -> PriceDimensions
buildPriceDimensions priceDimensions =
    PriceDimensions (List.map (\(name, priceDimension) -> { priceDimension | name = name }) priceDimensions)


unknownPriceDimensionsKeyDecoder : Decoder PriceDimensions
unknownPriceDimensionsKeyDecoder =
   keyValuePairs priceDimensionDecoder
        |> map buildPriceDimensions


priceDimensionDecoder : Decoder PriceDimension
priceDimensionDecoder =
    succeed PriceDimension
        |> hardcoded ""
        |> required "unit" string
        |> required "endRange" string
        |> required "description" string
        |> required "rateCode" string
        |> required "beginRange" string
        |> required "pricePerUnit" pricePerUnitDecoder


pricePerUnitDecoder : Decoder PricePerUnit
pricePerUnitDecoder = 
    succeed PricePerUnit
        |> optional "USD" string "N/A"