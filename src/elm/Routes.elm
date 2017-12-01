module Routes exposing (..)

import UrlParser exposing ((</>), map, oneOf, parsePath, s)

type Route
    = MyPositions
    | SignIn

parse msg location =
    parsePath route location
        |> msg

route =
    oneOf
        [ map MyPositions ( s "" )
        , map SignIn ( s "sign-in" )
        ]
