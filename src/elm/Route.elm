module Route exposing (..)

import UrlParser exposing ((</>), map, oneOf, parseHash, s)

type ProtectedRoute
    = MyPositions
    
type PublicRoute
    = SignIn

type Route
    = Protected ProtectedRoute 
    | Public PublicRoute

parse location =
    if String.isEmpty location.hash then
        Just (Protected MyPositions)
    else 
        parseHash route location

route =
    oneOf
        [ map (Protected MyPositions) (s "")
        , map (Public SignIn) (s "signin")
        ]
