module Routes exposing (..)

import UrlParser exposing ((</>), map, oneOf, parsePath, s)

type Route
    = MyPositions
    | SignIn

parse location =
    parsePath route location

route =
    oneOf
        [ map MyPositions ( s "" )
        , map SignIn ( s "sign-in" )
        ]
