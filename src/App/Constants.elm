module App.Constants exposing (..)

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

