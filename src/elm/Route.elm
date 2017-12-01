module Route exposing (..)

import UrlParser exposing ((</>), map, oneOf, parseHash, s)

type Route
    = MyPositions
    | SignIn

parse location =
    if String.isEmpty location.hash then
        Just MyPositions
    else 
        parseHash route location

route =
    oneOf
        [ map MyPositions (s "")
        , map SignIn (s "signin")
        ]
