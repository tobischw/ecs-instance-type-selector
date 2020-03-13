module App.Configuration exposing (Container, Service, Task, view)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Container =
    { id : Int
    , name : String
    }


type alias Task =
    { id : Int
    , name : String
    , containers : List Container
    }


type alias Service =
    { id : Int
    , name : String
    , tasks : List Task
    }


viewServices : List Service -> List (ListGroup.CustomItem msg)
viewServices services =
    List.concatMap viewService services


viewService : Service -> List (ListGroup.CustomItem msg)
viewService service =
    List.concat
        [ [ listItem service.name "weather-cloudy" [ href ("/service/" ++ String.fromInt service.id) ]
          ]
        , List.concat (List.map viewTask service.tasks)
        ]


viewTask : Task -> List (ListGroup.CustomItem msg)
viewTask task =
    List.concat
        [ [ listItem task.name "clipboard" [ href ("/task/" ++ String.fromInt task.id), class "pl-4" ]
          ]
        , List.map viewContainer task.containers
        ]


viewContainer : Container -> ListGroup.CustomItem msg
viewContainer container =
    listItem container.name "archive" [ href ("/container/" ++ String.fromInt container.id), class "pl-5" ]


listItem : String -> String -> List (Html.Attribute msg) -> ListGroup.CustomItem msg
listItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ Util.icon icon, text label ]


view : List Service -> Grid.Column msg
view services =
    Grid.col [ Col.md3, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Configuration"
            , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ] ] [ text "Add Service" ]
            , ListGroup.custom (viewServices services)
            , hr [] []
            , ListGroup.custom
                [ listItem "Global Settings" "cog" [ href "../settings" ]
                , listItem "Export as JSON" "eject" [ href "#" ]
                , listItem "Load JSON" "download-outline" [ href "#" ]
                ]
            ]
        ]
