module App.Settings exposing (Model, Msg(..), init, subscriptions, update, view)
import App.Instances as Instances exposing (FilterType, Model, Filters, update, Msg(..), PreferredPricing(..))
import App.Constants exposing (instanceTypes, allRegions)
import App.Util as Util
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form as Form
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Multiselect
import App.Instances exposing (Instances)

type alias Model =
    { excludedInstances : Multiselect.Model
    , excludedSystems: Multiselect.Model
    , includedRegions: Multiselect.Model
    , preferredPricing: PreferredPricing
    }


init : Model
init =
    { excludedInstances = Multiselect.initModel (List.map (\instanceType -> (instanceType, String.toUpper instanceType)) instanceTypes) "A"
    , excludedSystems = Multiselect.initModel [("SUSE", "SUSE"), ("Windows", "Windows"), ("Linux", "Linux"), ("RHEL", "RHEL")] "B"
    , includedRegions = Multiselect.initModel (List.map (\region -> (region, region)) allRegions) "C"
    , preferredPricing = Reserved1Yr
    }



-- There's a better way to do this...


type Msg
    = UpdateExcludedInstances Multiselect.Msg
    | UpdateExcludedOS Multiselect.Msg
    | UpdateIncludedRegions Multiselect.Msg
    | SetPricingPreference PreferredPricing

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExcludedInstances instancesChangedMessage ->
            let
                ( newExcludedInstances, subCmd, _ ) =
                    Multiselect.update instancesChangedMessage model.excludedInstances
            in
            ( { model | excludedInstances = newExcludedInstances }, Cmd.map UpdateExcludedInstances subCmd )

        UpdateExcludedOS osChangedMessage ->
            let
                ( newExcludedos, subCmd, _ ) =
                    Multiselect.update osChangedMessage model.excludedSystems
            in
            ( { model | excludedSystems = newExcludedos}, Cmd.map UpdateExcludedOS subCmd )

        UpdateIncludedRegions regionsChangedMessage ->
            let
                ( newExcludedRegions, subCmd, _) =
                    Multiselect.update regionsChangedMessage model.includedRegions
            in
            ( { model | includedRegions = newExcludedRegions}, Cmd.map UpdateIncludedRegions subCmd )

        SetPricingPreference pref -> 
            ({model | preferredPricing = pref}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Sub.map UpdateExcludedInstances <| Multiselect.subscriptions model.excludedInstances
    , Sub.map UpdateExcludedOS <| Multiselect.subscriptions model.excludedSystems
    , Sub.map UpdateIncludedRegions <| Multiselect.subscriptions model.includedRegions
    ]
    


view : Model -> Html Msg
view model =
    Card.config []
        |> Card.header [] [ text "Filters" ]
        |> Card.block []
            [ Block.custom <|
                Form.form []
                    [ Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Instance Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedInstances <| Multiselect.view model.excludedInstances
                            , Form.help [] [ text "Exclude specific ECS instances. These will be ignored during the cluster optimization calculation." ]
                            ]
                        ]
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Excluded Operating System Types" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateExcludedOS <| Multiselect.view model.excludedSystems
                            , Form.help [] [ text "Exclude specific operating systems from the EC2 instances." ]
                            ]
                        ]
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Included Regions" ]
                        , Form.col [ Col.sm9 ]
                            [ Html.map UpdateIncludedRegions <| Multiselect.view model.includedRegions
                            ]
                        ]
                    , hr [] []
                    , Form.row []
                        [ Form.colLabel [ Col.sm3 ] [ text "Pricing Preference" ]
                        , Form.col [ Col.sm9 ]
                            [ Fieldset.config
                                |> Fieldset.asGroup
                                |> Fieldset.children
                                    (Radio.radioList "pricingOptions"
                                        [ Radio.create [ Radio.id "reserved_1yr", Radio.checked (model.preferredPricing == Reserved1Yr), Radio.onClick (SetPricingPreference Reserved1Yr) ] "Reserved (1 year)"
                                        , Radio.create [ Radio.id "reserved_3yr", Radio.checked (model.preferredPricing == Reserved3Yr), Radio.onClick (SetPricingPreference Reserved3Yr) ] "Reserved (3 year)"
                                        , Radio.create [ Radio.id "ondemand", Radio.checked (model.preferredPricing == OnDemandPricing), Radio.onClick (SetPricingPreference OnDemandPricing) ] "On-Demand"
                                        ]
                                    )
                                |> Fieldset.view
                            ]
                        ]
                    ]
            ]
        |> Card.view
