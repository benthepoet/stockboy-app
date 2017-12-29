module Route exposing (..)

import UrlParser exposing ((</>), int, map, oneOf, parseHash, s)

type ProtectedRoute
    = MyPositions
    | StockPosition Int
    
type PublicRoute
    = NotFound
    | SignIn

type Route
    = Protected ProtectedRoute 
    | Public PublicRoute

toPath route =
    case route of
        Protected MyPositions ->
            "#/"
    
        Protected (StockPosition id) ->
            "#/stockposition/" ++ (toString id)
    
        Public SignIn ->
            "#/signin"
            
        Public NotFound ->
            "#/notfound"

parse location =
    if String.isEmpty location.hash then
        Protected MyPositions
    else 
        Maybe.withDefault (Public NotFound) (parseHash route location)

route =
    oneOf
        [ map (Protected MyPositions) (s "")
        , map (Protected << StockPosition) (s "stockposition" </> int)
        , map (Public SignIn) (s "signin")
        ]
