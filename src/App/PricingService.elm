port module App.PricingService exposing (getProducts, receiveProducts)


port getProducts : ( String, Int ) -> Cmd msg

port receiveProducts : (String -> msg) -> Sub msg
