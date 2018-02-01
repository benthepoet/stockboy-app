module Widgets exposing (..)

import Html exposing (p, span, text)
import Html.Attributes as Attributes

staticField label value =
    p []
        [ text label
        , span [ Attributes.class "float" ] [ text value ]
        ]