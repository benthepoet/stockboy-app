module Widgets exposing (..)

import Html exposing (a, i, p, span, text)
import Html.Attributes as Attributes
import Html.Events as Events

menuButton label icon msg =
    a
        [ Attributes.class "button"
        , Events.onClick msg
        ]
        [ i [ Attributes.class <| "fas fa-" ++ icon ] []
        , text label
        ]

staticField label value =
    p []
        [ text label
        , span [ Attributes.class "float" ] [ text value ]
        ]