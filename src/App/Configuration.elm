module App.Configuration exposing (Container, Msg(..), Service, Task, view, init, update, Model)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Html exposing (..)
import Html.Attributes exposing (..)


init : Model
init =
    { services = [ Service 0 "Service a" [ Task 0 "Task a1" [ Container 0 "Container a11" ] ], Service 1 "Tobi's Cool Service" [], Service 2 "Will's Garbage Service" [] ] }


type alias Services =
    List Service


type alias Model =
    { services : Services
    }


type Msg
    = AddService


type alias Service =
    { id : Int
    , name : String
    , tasks : List Task
    }


type alias Container =
    { id : Int
    , name : String
    }


type alias Task =
    { id : Int
    , name : String
    , containers : List Container
    }   


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddService ->
            { model | services = model.services ++ [ Service 5 "Test Service" [] ] }


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


view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Configuration"
        , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ], Button.onClick AddService ] [ text "Add Service" ]
        , ListGroup.custom (viewServices model.services)
        , hr [] []
        , ListGroup.custom
            [ listItem "Global Settings" "cog" [ href "../settings" ]
            , listItem "Export as JSON" "eject" [ href "#" ]
            , listItem "Load JSON" "download-outline" [ href "#" ]
            ]
        ]
