module Main exposing (..)

import Browser
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


view : Model -> Html Msg
view model =
    div []
        [
            nav [class "navbar navbar-dark fixed-top bg-dark flex-md-nowrap p-0 shadow"] [
                a [class "navbar-brand col-sm-3 col-md-3 mr-0 text-center"] [
                    img [class "logo", src "logo.png"] [],
                    text "Cluster Prophet"
                ],
                ul [class "navbar-nav px-3"] [
                    li [class "nav-item text-nowrap"] [
                        a [class "nav-link"] [text "Resync"]
                    ]
                ]
            ],
            div [class "container-fluid"] [
                div [class "row"] [
                    nav [class "col-md-3 d-none d-md-block bg-light sidebar"] [
                        div [class "sidebar-sticky"] [
                            h6 [class "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-3 mb-1 text-muted"] [
                                span [] [text "Configuration"]
                            ],
                            ul [class "nav flex-column"] [
                                li [class "nav-item"] [
                                    a [class "nav-link active"] [
                                        text "Unnamed Cluster"
                                    ]
                                ],
                                li [class "nav-item pl-3"] [
                                    a [class "nav-link"] [
                                        text "Unnamed Service"
                                    ]
                                ],
                                li [class "nav-item pl-5"] [
                                    a [class "nav-link"] [
                                        text "Memory-Optimized Task"
                                    ]
                                ],
                                li [class "nav-item pl-5"] [
                                    a [class "nav-link"] [
                                        text "General Purpose Task"
                                    ]
                                ],
                                li [class "nav-item pl-3"] [
                                    a [class "nav-link"] [
                                        text "Tomcat Service"
                                    ]
                                ],
                                li [class "nav-item pl-3"] [
                                    a [class "nav-link"] [
                                        text "Storage Optimized Task"
                                    ]
                                ]
                            ],
                            div [class "bottom-nav"] [
                                button [class "btn btn-success btn-lg btn-block m-2"] [text "Optimize"],
                                hr [] [],
                                div [class "text-right text-muted"] [
                                    h3 [] [text "$0.00000"],
                                    text "per hour"
                                ],
                                small [] [ text "TODO: when optimize clicked, show results of top 5 best configs"]
                            ]
                        ]
                    ],
                    div [class "col-md-8 ml-sm-auto col-lg-9 px-4 mt-5"] [
                        div [class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"] [
                            h1 [class "h3"] [text "Unnamed Cluster"],
                            div [class "btn-toolbar mb-2 mb-md-0"] [
                                div [class "btn-group mr-2"] [
                                    button [class "btn btn-sm btn-outline-secondary"] [text "Import"],
                                    button [class "btn btn-sm btn-outline-secondary dropdown-toggle"] [text "Recent"]
                                ],
                                div [class "btn-group mr-2"] [
                                    button [class "btn btn-sm btn-outline-secondary"] [text "Save"],
                                    button [class "btn btn-sm btn-outline-secondary"] [text "Export"]
                                ],
                                button [class "btn btn-sm btn-outline-secondary dropdown-toggle"] [text "Templates"]
                            ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
