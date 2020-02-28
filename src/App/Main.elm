module App.Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "ECS Instance Selector"
    , body =
  [ nav [ class "navbar navbar-dark fixed-top bg-dark flex-md-nowrap p-0 shadow" ]
    [ a [ class "navbar-brand col-sm-3 col-md-3 mr-0 text-center", href "#" ]
      [ img [ class "logo", src "logo.png" ]
        []
      , text "Cluster Prophet"
      ]
    , ul [ class "navbar-nav px-3" ]
      [ li [ class "nav-item text-nowrap" ]
        [ a [ class "nav-link", href "#" ]
          [ text "Resync" ]
        ]
      ]
    ]
  , div [ class "container-fluid" ]
    [ div [ class "row" ]
      [ nav [ class "col-md-3 d-none d-md-block bg-light sidebar" ]
        [ div [ class "sidebar-sticky" ]
          [ h6 [ class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-3 mb-1 text-muted" ]
            [ span []
              [ text "Configuration" ]
            ]
          , ul [ class "nav flex-column" ]
            [ li [ class "nav-item" ]
              [ a [ class "nav-link active", href "#" ]
                [ span [ attribute "data-feather" "server" ]
                  []
                , text "Unnamed Cluster "
                , span [ class "sr-only" ]
                  [ text "(current)" ]
                ]
              ]
            , li [ class "nav-item pl-3" ]
              [ a [ class "nav-link", href "#" ]
                [ span [ attribute "data-feather" "box" ]
                  []
                , text "Unnamed Service                            "
                ]
              ]
            , li [ class "nav-item pl-5" ]
              [ a [ class "nav-link", href "#" ]
                [ span [ attribute "data-feather" "terminal" ]
                  []
                , text "Memory-Optimized Task                            "
                ]
              ]
            , li [ class "nav-item pl-5" ]
              [ a [ class "nav-link", href "#" ]
                [ span [ attribute "data-feather" "terminal" ]
                  []
                , text "General Purpose Task                            "
                ]
              ]
            , li [ class "nav-item pl-3" ]
              [ a [ class "nav-link", href "#" ]
                [ span [ attribute "data-feather" "box" ]
                  []
                , text "Tomcat Service                            "
                ]
              ]
            , li [ class "nav-item pl-5" ]
              [ a [ class "nav-link", href "#" ]
                [ span [ attribute "data-feather" "terminal" ]
                  []
                , text "Storage-Optimized Task                            "
                ]
              ]
            ]
          , div [ class "bottom-nav" ]
            [ button [ class "btn btn-success btn-lg btn-block m-2" ]
              [ text "Optimize" ]
            , hr []
              []
            , div [ class "m-2 mb-3" ]
              [ div [ class "text-right text-muted" ]
                [ h3 []
                  [ text "$0.00000" ]
                , text "per hour                                                            "
                ]
              , small []
                [ text "TODO: when optimize clicked, show results of top 5 best configs" ]
              ]
            ]
          ]
        ]
      , main_ [ class "col-md-8 ml-sm-auto col-lg-9 px-4", attribute "role" "main" ]
        [ div [ class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom" ]
          [ h1 [ class "h3" ]
            [ text "Unnamed Cluster" ]
          , div [ class "btn-toolbar mb-2 mb-md-0" ]
            [ div [ class "btn-group mr-2" ]
              [ button [ class "btn btn-sm btn-outline-secondary", type_ "button" ]
                [ text "Import" ]
              , button [ class "btn btn-sm btn-outline-secondary dropdown-toggle", type_ "button" ]
                [ text "Recent                            " ]
              ]
            , div [ class "btn-group mr-2" ]
              [ button [ class "btn btn-sm btn-outline-secondary", type_ "button" ]
                [ span [ attribute "data-feather" "save" ]
                  []
                , text "Save"
                ]
              , button [ class "btn btn-sm btn-outline-secondary", type_ "button" ]
                [ span [ attribute "data-feather" "share" ]
                  []
                , text "Export"
                ]
              ]
            , button [ class "btn btn-sm btn-outline-secondary dropdown-toggle", type_ "button" ]
              [ span [ attribute "data-feather" "box" ]
                []
              , text "Templates                        "
              ]
            ]
          ]
        , div [ class "container-flex" ]
          [ div [ class "row" ]
            [ div [ class "col-lg-12" ]
              [ div [ class "card" ]
                [ div [ class "card-header" ]
                  [ div [ class "d-flex align-items-center" ]
                    [ div [ class "mr-auto" ]
                      [ h5 []
                        [ text "Unnamed Service" ]
                      , span [ class "text-muted" ]
                        [ text "2 tasks · Created 20                                                minutes ago" ]
                      , text "·                                            "
                      , button [ attribute "aria-controls" "collapseExample", attribute "aria-expanded" "false", class "btn btn-link btn-sm p-0", attribute "data-target" "#collapseExample2", attribute "data-toggle" "collapse", type_ "button" ]
                        [ span [ attribute "data-feather" "chevron-down" ]
                          []
                        , text "Configure                                            "
                        ]
                      ]
                    , div [ class "btn-group", attribute "role" "group" ]
                      [ button [ class "btn btn-outline-secondary btn-sm" ]
                        [ span [ attribute "data-feather" "copy" ]
                          [ p []
                            []
                          ]
                        ]
                      , button [ class "btn btn-outline-secondary btn-sm" ]
                        [ span [ attribute "data-feather" "eye-off" ]
                          []
                        ]
                      , button [ class "btn btn-outline-danger btn-sm" ]
                        [ span [ attribute "data-feather" "x" ]
                          []
                        ]
                      ]
                    ]
                  , div [ class "collapse", id "collapseExample2" ]
                    [ hr []
                      []
                    , Html.form []
                      [ div [ class "form-group row" ]
                        [ label [ class "col-sm-3 col-form-label" ]
                          [ span [ attribute "data-feather" "percent" ]
                            []
                          , text "Scale tasks                                                    "
                          , br []
                            []
                          , span [ class "text-muted" ]
                            [ text "(is this                                                        right?)" ]
                          ]
                        , div [ class "col-sm-9 mt-2" ]
                          [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                            []
                          ]
                        ]
                      , hr []
                        []
                      , div [ class "form-check" ]
                        [ input [ attribute "checked" "checked", class "form-check-input", id "exampleRadios1", name "exampleRadios", type_ "radio", value "option1" ]
                          []
                        , label [ class "form-check-label", for "exampleRadios1" ]
                          [ text "Replicated Service                                                " ]
                        ]
                      , div [ class "form-check" ]
                        [ input [ class "form-check-input", id "exampleRadios2", name "exampleRadios", type_ "radio", value "option2" ]
                          []
                        , label [ class "form-check-label", for "exampleRadios2" ]
                          [ text "Daemon Service                                                " ]
                        ]
                      ]
                    ]
                  ]
                , div [ class "card-body" ]
                  [ div [ class "container-flex" ]
                    [ div [ class "row" ]
                      [ div [ class "col-sm-6 p-4" ]
                        [ div [ class "card", attribute "style" "box-shadow: 0 2px 25px rgba(0,0,0,0.1)" ]
                          [ div [ class "card-header" ]
                            [ div [ class "d-flex align-items-center" ]
                              [ div [ class "mr-auto" ]
                                [ span [ class " font-weight-bold" ]
                                  [ text "Memory-Optimized                                                                    Task" ]
                                , text "· Task #1                                                            "
                                ]
                              , div [ class "btn-group", attribute "role" "group" ]
                                [ button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "copy" ]
                                    [ p []
                                      []
                                    ]
                                  ]
                                , button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "eye-off" ]
                                    []
                                  ]
                                , button [ class "btn btn-outline-danger btn-sm" ]
                                  [ span [ attribute "data-feather" "x" ]
                                    []
                                  ]
                                ]
                              ]
                            ]
                          , div [ class "card-body" ]
                            [ Html.form []
                              [ select [ class "form-control form-control-md" ]
                                [ option [ attribute "selected" "selected" ]
                                  [ text "General Purpose" ]
                                , option []
                                  [ text "Compute Optimized" ]
                                , option []
                                  [ text "Memory Optimized" ]
                                , option []
                                  [ text "Accelerated Computing" ]
                                , option []
                                  [ text "Storage Optimized" ]
                                ]
                              , div [ class "form-group row mt-2" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "cpu" ]
                                    []
                                  , text "CPU"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "layers" ]
                                    []
                                  , text "Memory"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "database" ]
                                    []
                                  , text "Storage"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , hr []
                                []
                              , button [ attribute "aria-controls" "collapseExample", attribute "aria-expanded" "false", class "btn btn-link btn-block btn-sm", attribute "data-target" "#collapseExample1", attribute "data-toggle" "collapse", type_ "button" ]
                                [ span [ attribute "data-feather" "chevron-down" ]
                                  []
                                , text "More                                                            "
                                ]
                              , div [ class "collapse", id "collapseExample1" ]
                                [ hr []
                                  []
                                , div [ class "form-group row" ]
                                  [ label [ class "col-sm-4 col-form-label" ]
                                    [ span [ attribute "data-feather" "percent" ]
                                      []
                                    , text "Scale tasks                                                                        "
                                    , br []
                                      []
                                    , span [ class "text-muted" ]
                                      [ text "(is this                                                                            right?)" ]
                                    ]
                                  , div [ class "col-sm-8 mt-2" ]
                                    [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                      []
                                    ]
                                  ]
                                , hr []
                                  []
                                , label [ class "form-check" ]
                                  [ input [ class "form-check-input", id "defaultCheck1", type_ "checkbox", value "" ]
                                    []
                                  , label [ class "form-check-label", for "defaultCheck1" ]
                                    [ text "Account for Availability Zones (AZs)                                                                    " ]
                                  ]
                                , hr []
                                  []
                                , div [ class "form-check" ]
                                  [ input [ attribute "checked" "checked", class "form-check-input", id "exampleRadios1", name "exampleRadios", type_ "radio", value "option1" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios1" ]
                                    [ text "Prioritize packing by memory                                                                    " ]
                                  ]
                                , div [ class "form-check" ]
                                  [ input [ class "form-check-input", id "exampleRadios2", name "exampleRadios", type_ "radio", value "option2" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios2" ]
                                    [ text "Prioritize packing by CPU                                                                    " ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      , div [ class "col-sm-6 p-4" ]
                        [ div [ class "card", attribute "style" "box-shadow: 0 2px 25px rgba(0,0,0,0.1)" ]
                          [ div [ class "card-header" ]
                            [ div [ class "d-flex align-items-center" ]
                              [ div [ class "mr-auto" ]
                                [ span [ class " font-weight-bold" ]
                                  [ text "General Purpose                                                                    Task" ]
                                , text "· Task #2                                                            "
                                ]
                              , div [ class "btn-group", attribute "role" "group" ]
                                [ button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "copy" ]
                                    [ p []
                                      []
                                    ]
                                  ]
                                , button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "eye-off" ]
                                    []
                                  ]
                                , button [ class "btn btn-outline-danger btn-sm" ]
                                  [ span [ attribute "data-feather" "x" ]
                                    []
                                  ]
                                ]
                              ]
                            ]
                          , div [ class "card-body" ]
                            [ Html.form []
                              [ select [ class "form-control form-control-md" ]
                                [ option [ attribute "selected" "selected" ]
                                  [ text "General Purpose" ]
                                , option []
                                  [ text "Compute Optimized" ]
                                , option []
                                  [ text "Memory Optimized" ]
                                , option []
                                  [ text "Accelerated Computing" ]
                                , option []
                                  [ text "Storage Optimized" ]
                                ]
                              , div [ class "form-group row mt-2" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "cpu" ]
                                    []
                                  , text "CPU"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "layers" ]
                                    []
                                  , text "Memory"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "database" ]
                                    []
                                  , text "Storage"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , hr []
                                []
                              , button [ attribute "aria-controls" "collapseExample", attribute "aria-expanded" "false", class "btn btn-link btn-block btn-sm", attribute "data-target" "#collapseExample3", attribute "data-toggle" "collapse", type_ "button" ]
                                [ span [ attribute "data-feather" "chevron-down" ]
                                  []
                                , text "More                                                            "
                                ]
                              , div [ class "collapse", id "collapseExample3" ]
                                [ hr []
                                  []
                                , div [ class "form-group row" ]
                                  [ label [ class "col-sm-4 col-form-label" ]
                                    [ span [ attribute "data-feather" "percent" ]
                                      []
                                    , text "Scale tasks                                                                        "
                                    , br []
                                      []
                                    , span [ class "text-muted" ]
                                      [ text "(is this                                                                            right?)" ]
                                    ]
                                  , div [ class "col-sm-8 mt-2" ]
                                    [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                      []
                                    ]
                                  ]
                                , hr []
                                  []
                                , label [ class "form-check" ]
                                  [ input [ class "form-check-input", id "defaultCheck1", type_ "checkbox", value "" ]
                                    []
                                  , label [ class "form-check-label", for "defaultCheck1" ]
                                    [ text "Account for Availability Zones (AZs)                                                                    " ]
                                  ]
                                , hr []
                                  []
                                , div [ class "form-check" ]
                                  [ input [ attribute "checked" "checked", class "form-check-input", id "exampleRadios1", name "exampleRadios", type_ "radio", value "option1" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios1" ]
                                    [ text "Prioritize packing by memory                                                                    " ]
                                  ]
                                , div [ class "form-check" ]
                                  [ input [ class "form-check-input", id "exampleRadios2", name "exampleRadios", type_ "radio", value "option2" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios2" ]
                                    [ text "Prioritize packing by CPU                                                                    " ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    , div [ class "row p-3" ]
                      [ a [ class "btn btn-block btn-link", href "#" ]
                        [ span [ attribute "data-feather" "plus" ]
                          []
                        , text "Add Task"
                        ]
                      ]
                    ]
                  ]
                , div [ class "card-footer" ]
                  [ small [ class "text-muted" ]
                    [ text "Replicated: Yes · Packing impact: "
                    , strong []
                      [ text "Low" ]
                    , text "·                                        No recommendations"
                    ]
                  ]
                ]
              ]
            ]
          , div [ class "row mt-4" ]
            [ div [ class "col-lg-12" ]
              [ div [ class "card" ]
                [ div [ class "card-header" ]
                  [ div [ class "d-flex align-items-center" ]
                    [ div [ class "mr-auto" ]
                      [ h5 []
                        [ text "Tomcat Service" ]
                      , span [ class "text-muted" ]
                        [ text "1 tasks · Created 3                                                days ago" ]
                      , text "·                                            "
                      , button [ attribute "aria-controls" "collapseExample", attribute "aria-expanded" "false", class "btn btn-link btn-sm p-0", attribute "data-target" "#collapseExample4", attribute "data-toggle" "collapse", type_ "button" ]
                        [ span [ attribute "data-feather" "chevron-down" ]
                          []
                        , text "Configure                                            "
                        ]
                      ]
                    , div [ class "btn-group", attribute "role" "group" ]
                      [ button [ class "btn btn-outline-secondary btn-sm" ]
                        [ span [ attribute "data-feather" "copy" ]
                          [ p []
                            []
                          ]
                        ]
                      , button [ class "btn btn-outline-secondary btn-sm" ]
                        [ span [ attribute "data-feather" "eye-off" ]
                          []
                        ]
                      , button [ class "btn btn-outline-danger btn-sm" ]
                        [ span [ attribute "data-feather" "x" ]
                          []
                        ]
                      ]
                    ]
                  , div [ class "collapse", id "collapseExample4" ]
                    [ hr []
                      []
                    , Html.form []
                      [ div [ class "form-group row" ]
                        [ label [ class "col-sm-3 col-form-label" ]
                          [ span [ attribute "data-feather" "percent" ]
                            []
                          , text "Scale tasks"
                          , br []
                            []
                          , span [ class "text-muted" ]
                            [ text "(is this right?)" ]
                          ]
                        , div [ class "col-sm-9 mt-2" ]
                          [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                            []
                          ]
                        ]
                      , hr []
                        []
                      , div [ class "form-check" ]
                        [ input [ attribute "checked" "checked", class "form-check-input", id "exampleRadios1", name "exampleRadios", type_ "radio", value "option1" ]
                          []
                        , label [ class "form-check-label", for "exampleRadios1" ]
                          [ text "Replicated Service                                                " ]
                        ]
                      , div [ class "form-check" ]
                        [ input [ class "form-check-input", id "exampleRadios2", name "exampleRadios", type_ "radio", value "option2" ]
                          []
                        , label [ class "form-check-label", for "exampleRadios2" ]
                          [ text "Daemon Service                                                " ]
                        ]
                      ]
                    ]
                  ]
                , div [ class "card-body" ]
                  [ div [ class "container-flex" ]
                    [ div [ class "row" ]
                      [ div [ class "col-sm-6 p-4" ]
                        [ div [ class "card", attribute "style" "box-shadow: 0 2px 25px rgba(0,0,0,0.1)" ]
                          [ div [ class "card-header" ]
                            [ div [ class "d-flex align-items-center" ]
                              [ div [ class "mr-auto" ]
                                [ span [ class " font-weight-bold" ]
                                  [ text "Storage-Optimized                                                                    Task" ]
                                , text "· Task #1                                                            "
                                ]
                              , div [ class "btn-group", attribute "role" "group" ]
                                [ button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "copy" ]
                                    [ p []
                                      []
                                    ]
                                  ]
                                , button [ class "btn btn-outline-secondary btn-sm" ]
                                  [ span [ attribute "data-feather" "eye-off" ]
                                    []
                                  ]
                                , button [ class "btn btn-outline-danger btn-sm" ]
                                  [ span [ attribute "data-feather" "x" ]
                                    []
                                  ]
                                ]
                              ]
                            ]
                          , div [ class "card-body" ]
                            [ Html.form []
                              [ select [ class "form-control form-control-md" ]
                                [ option [ attribute "selected" "selected" ]
                                  [ text "General Purpose" ]
                                , option []
                                  [ text "Compute Optimized" ]
                                , option []
                                  [ text "Memory Optimized" ]
                                , option []
                                  [ text "Accelerated Computing" ]
                                , option []
                                  [ text "Storage Optimized" ]
                                ]
                              , div [ class "form-group row mt-2" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "cpu" ]
                                    []
                                  , text "CPU"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "layers" ]
                                    []
                                  , text "Memory"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , div [ class "form-group row" ]
                                [ label [ class "col-sm-4 col-form-label" ]
                                  [ span [ attribute "data-feather" "database" ]
                                    []
                                  , text "Storage"
                                  ]
                                , div [ class "col-sm-8 mt-2" ]
                                  [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                    []
                                  ]
                                ]
                              , hr []
                                []
                              , button [ attribute "aria-controls" "collapseExample", attribute "aria-expanded" "false", class "btn btn-link btn-block btn-sm", attribute "data-target" "#collapseExample5", attribute "data-toggle" "collapse", type_ "button" ]
                                [ span [ attribute "data-feather" "chevron-down" ]
                                  []
                                , text "More                                                            "
                                ]
                              , div [ class "collapse", id "collapseExample5" ]
                                [ hr []
                                  []
                                , div [ class "form-group row" ]
                                  [ label [ class "col-sm-4 col-form-label" ]
                                    [ span [ attribute "data-feather" "percent" ]
                                      []
                                    , text "Scale tasks                                                                        "
                                    , br []
                                      []
                                    , span [ class "text-muted" ]
                                      [ text "(is this                                                                            right?)" ]
                                    ]
                                  , div [ class "col-sm-8 mt-2" ]
                                    [ input [ class "form-control-range custom-range", id "formControlRange", type_ "range", value "50" ]
                                      []
                                    ]
                                  ]
                                , hr []
                                  []
                                , label [ class "form-check" ]
                                  [ input [ class "form-check-input", id "defaultCheck1", type_ "checkbox", value "" ]
                                    []
                                  , label [ class "form-check-label", for "defaultCheck1" ]
                                    [ text "Account for Availability Zones (AZs)                                                                    " ]
                                  ]
                                , hr []
                                  []
                                , div [ class "form-check" ]
                                  [ input [ attribute "checked" "checked", class "form-check-input", id "exampleRadios1", name "exampleRadios", type_ "radio", value "option1" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios1" ]
                                    [ text "Prioritize packing by memory                                                                    " ]
                                  ]
                                , div [ class "form-check" ]
                                  [ input [ class "form-check-input", id "exampleRadios2", name "exampleRadios", type_ "radio", value "option2" ]
                                    []
                                  , label [ class "form-check-label", for "exampleRadios2" ]
                                    [ text "Prioritize packing by CPU                                                                    " ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    , div [ class "row p-3" ]
                      [ a [ class "btn btn-block btn-link", href "#" ]
                        [ span [ attribute "data-feather" "plus" ]
                          []
                        , text "Add Task"
                        ]
                      ]
                    ]
                  ]
                , div [ class "card-footer" ]
                  [ small [ class "text-muted" ]
                    [ text "Replicated: Yes · Packing impact: "
                    , strong []
                      [ text "Low" ]
                    , text "·                                        No recommendations"
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
        ]
    }


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }