module App.Configuration exposing (Container, Model, Msg(..), Service, Task, init, update, view)

import App.Util as Util
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple exposing (first, second)
import Html.Events.Extra exposing (onChange, onEnter)


init : Model
init =
    { services = Dict.fromList [ ( 0, Service "Service A" 50 (Dict.fromList [ (0, Task "Task A" 20 Dict.empty) ]) ) ]
    , newServiceModal = Modal.hidden
    , newServiceName = ""
    }


type alias Services =
    Dict Int Service


type alias Model =
    { services : Services
    , newServiceModal : Modal.Visibility
    , newServiceName : String
    }


type Msg
    = AddService
    | CloseModal
    | ShowModal
    | ChangeNewServiceName String


type alias Service =
    { name : String
    , scalingTarget : Int
    , tasks : Dict Int Task
    }


type alias Task =
    { name : String
    , totalMemory : Int
    , containers : Dict Int Container
    }

type alias Container =
    { name : String
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddService ->
            let
                name =
                    if String.isEmpty model.newServiceName then
                        "Unnamed Service"

                    else
                        model.newServiceName

                id = Dict.size model.services
            in
            { model | services = model.services |> Dict.insert id (Service name 50 Dict.empty), newServiceName = "", newServiceModal = Modal.hidden }

        CloseModal ->
            { model | newServiceModal = Modal.hidden, newServiceName = "" }

        ShowModal ->
            { model | newServiceModal = Modal.shown }

        ChangeNewServiceName newName ->
            { model | newServiceName = newName }



-- rewrite these view functions, use Dict.map?


viewServices : Services -> List (ListGroup.CustomItem msg)
viewServices services =
    List.concatMap viewService (Dict.toList services)


viewService : ( Int, Service ) -> List (ListGroup.CustomItem msg)
viewService serviceWithId =
    let
        service =
            second serviceWithId
    in
    List.concat
        [ [ listItem service.name "weather-cloudy" [ href ("/service/" ++ String.fromInt (first serviceWithId)) ]
          ]
        , List.concat (List.map (viewTask (first serviceWithId)) (Dict.toList service.tasks))
        ]


viewTask : Int -> (Int, Task) -> List (ListGroup.CustomItem msg)
viewTask serviceId taskWithId =
    let
        task =
            second taskWithId
    in
    List.concat
        [ [ listItem task.name "clipboard" [ href ("/service/" ++ String.fromInt serviceId ++ "/task/" ++ String.fromInt (first taskWithId)), class "pl-4" ]
          ]
        , List.map viewContainer (Dict.toList task.containers)
        ]


viewContainer : (Int, Container) -> ListGroup.CustomItem msg
viewContainer containerWithId =
    let
        container =
            second containerWithId
    in
    listItem container.name "archive" [ href ("/container/" ++ String.fromInt (first containerWithId)), class "pl-5" ]


viewNewServiceModal : Model -> Html Msg
viewNewServiceModal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "New Service" ]
        |> Modal.body []
            [ Form.form []
                [ Form.group []
                    [ Form.label [] [ text "Name:" ]
                    , Input.text [ Input.value model.newServiceName, Input.onInput ChangeNewServiceName, Input.attrs [ placeholder "Service Name"{-, onEnter ChangeNewServiceName-} ] ]
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.onClick CloseModal
                ]
                [ text "Cancel" ]
            , Button.button
                [ Button.success
                , Button.onClick AddService
                ]
                [ text "Add" ]
            ]
        |> Modal.view model.newServiceModal


listItem : String -> String -> List (Html.Attribute msg) -> ListGroup.CustomItem msg
listItem label icon attrs =
    ListGroup.anchor [ ListGroup.attrs attrs ] [ Util.icon icon, text label ]
    

view : Model -> Html Msg
view model =
    div [ class "px-3", class "pt-1" ]
        [ Util.viewColumnTitle "Configuration"
        , Button.button [ Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2" ], Button.onClick ShowModal ] [ text "Add Service" ]
        , ListGroup.custom (viewServices model.services)
        , hr [] []
        , ListGroup.custom
            [ listItem "Global Settings" "cog" [ href "../settings" ]
            , listItem "Export as JSON" "eject" [ href "#" ]
            , listItem "Load JSON" "download-outline" [ href "#" ]
            ]
        , viewNewServiceModal model
        ]
