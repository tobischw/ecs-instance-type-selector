module App.Configuration exposing (view)

import App.Util as Util
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)


view : Grid.Column msg
view =
    Grid.col [ Col.md3, Col.attrs [ class "p-0 bg-light sidebar" ] ]
        [ div [ class "px-3", class "pt-1" ]
            [ Util.viewColumnTitle "Configuration"
            , Button.button [Button.outlineSuccess, Button.block, Button.attrs [ class "mb-2"]] [ text "Add Service"]
            , ListGroup.custom
                {- These will have to be rendered from the model AND replace with listItem func. -}
                [ ListGroup.anchor [ ListGroup.attrs [ href "service" ] ] [ Util.icon "weather-cloudy", text "Service A" ]
                , ListGroup.anchor [ ListGroup.attrs [ class "pl-4", href "task" ] ] [ Util.icon "clipboard", text "Task A" ]
                , ListGroup.anchor [ ListGroup.attrs [ class "pl-5", href "container" ] ] [ Util.icon "archive", text "Container 1a" ]
                , ListGroup.anchor [ ListGroup.attrs [ class "pl-5", href "container" ] ] [ Util.icon "archive", text "Container 2a" ]
                , ListGroup.anchor [ ListGroup.attrs [ href "service" ] ] [ Util.icon "weather-cloudy", text "Service B" ]
                ]
            , hr[] []
            , ListGroup.custom
                [ listItem "Global Settings" "cog" "settings"
                , listItem "Export as JSON" "eject" "#"
                , listItem "Load JSON" "download-outline" "settings"
                ]
            ]
        ]

listItem : String -> String -> String -> ListGroup.CustomItem msg
listItem label icon link =
    ListGroup.anchor [ ListGroup.attrs [ href link ] ] [ Util.icon icon, text label ]
