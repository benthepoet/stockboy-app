module Route exposing (..)

import UrlParser exposing ((</>), map, oneOf, parseHash, s)

type ProtectedRoute
    = MyPositions
    
type PublicRoute
    = SignIn
    | NotFound

type Route
    = Protected ProtectedRoute 
    | Public PublicRoute

parse location =
    if String.isEmpty location.hash then
        Protected MyPositions
    else 
        Maybe.withDefault (Public NotFound) (parseHash route location)

route =
    oneOf
        [ map (Protected MyPositions) (s "")
        , map (Public SignIn) (s "signin")
        ]
