module Route exposing (..)

import UrlParser exposing ((</>), int, map, oneOf, parseHash, s)


type ProtectedRoute
    = MyPositions
    | StockPosition Int
    | StockList


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

        Protected StockList ->
            "#/stocks"

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
        , map (Protected StockList) (s "stocks")
        , map (Public SignIn) (s "signin")
        ]
