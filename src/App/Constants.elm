module App.Constants exposing (..)


instanceTypes : List ( String, String )
instanceTypes =
    [ ( "t3", "T3" )
    , ( "t2", "T2" )
    , ( "m6", "M6" )
    , ( "m5", "M5" )
    , ( "m4", "M4" )
    , ( "m3", "M3" )
    , ( "m1", "m1" )
    , ( "h1", "H1" )
    , ( "d2", "D2" )
    , ( "cr1", "CR1" )
    , ( "u", "U")
    , ( "c6", "C6" )
    , ( "c5", "C5" )
    , ( "c4", "C4" )
    , ( "c3", "C3" )
    , ( "x1", "X1" )
    , ( "z1", "Z1" )
    , ( "a1", "A1" )
    , ( "r6", "R6" )
    , ( "r5", "R5" )
    , ( "r4", "R4" )
    , ( "r3", "R3" )
    , ( "p3", "P3" )
    , ( "p2", "P2" )
    , ( "g4", "G4" )
    , ( "g3", "G3" )
    , ( "g2", "G2" )
    , ( "f1", "F1" )
    , ( "inf1", "INF1" )
    , ( "i3", "I3" )
    , ( "i2", "I2" )
    ]


type alias RegionRecord =
    { regionCode : String
    , displayName : String
    , regionName : String
    }


allRegions : List RegionRecord
allRegions =
    [ RegionRecord "us" "US East (Ohio)" "us-east-2"
    , RegionRecord "us" "US East (N. Virginia)" "us-east-1"
    , RegionRecord "us" "US West (N. California)" "us-west-1"
    , RegionRecord "us" "US West (Oregon)" "us-west-2"
    , RegionRecord "ap" "Asia Pacific (Hong Kong)" "ap-east-1"
    , RegionRecord "ap" "Asia Pacific (Mumbai)" "ap-south-1"
    , RegionRecord "ap" "Asia Pacific (Osaka-Local)" "ap-northeast-3"
    , RegionRecord "ap" "Asia Pacific (Seoul)" "ap-northeast-2"
    , RegionRecord "ap" "Asia Pacific (Singapore)" "ap-southeast-1"
    , RegionRecord "ap" "Asia Pacific (Sydney)" "ap-southeast-2"
    , RegionRecord "ap" "Asia Pacific (Tokyo)" "ap-northeast-1"
    , RegionRecord "ca" "Canada (Central)" "ca-central-1"
    , RegionRecord "cn" "China (Beijing)" "cn-north-1"
    , RegionRecord "cn" "China (Ningxia)" "cn-northwest-1"
    , RegionRecord "eu" "Europe (Frankfurt)" "eu-central-1"
    , RegionRecord "eu" "Europe (Ireland)" "eu-west-1"
    , RegionRecord "eu" "Europe (London)" "eu-west-2"
    , RegionRecord "eu" "Europe (Paris)" "eu-west-3"
    , RegionRecord "eu" "Europe (Stockholm)" "eu-north-1"
    , RegionRecord "me" "Middle East (Bahrain)" "me-south-1"
    , RegionRecord "sa" "South America (Sao Paulo)" "sa-east-1"
    ]
