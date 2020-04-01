import Dict exposing (Dict)
import Dict.Extra exposing (find, filterMap)

type alias Cluster =
    {
        name : String
    }

type alias Service = 
    {
        clusterId : Int
        , name : String
    }

testDict =
    Dict.fromList [ ( 0, Service 0 "Test Service" ), ( 1, Service 1 "Test Service2" ) ]

let
  isAssociated n a =
    if a.clusterId == 0 then
        Just a
    else
        Nothing
in
    testDict |> filterMap isAssociated 