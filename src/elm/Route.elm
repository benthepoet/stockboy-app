module Route exposing (..)

import UrlParser exposing ((</>), int, map, oneOf, parseHash, s)


type ProtectedRoute
    = MyPositions
    | StockPosition Int
    | StockList


type PublicRoute
    = NotFound
    | SignIn
    | SignUp


type Route
    = Protected ProtectedRoute
    | Public PublicRoute


toPath route =
    case route of
        Protected MyPositions ->
            "#/"

        Protected (StockPosition id) ->
            "#/stock-position/" ++ (toString id)

        Protected StockList ->
            "#/stocks"

        Public SignIn ->
            "#/sign-in"

        Public SignUp ->
            "#/sign-up"

        Public NotFound ->
            "#/not-found"


parse location =
    if String.isEmpty location.hash then
        Protected MyPositions
    else
        Maybe.withDefault (Public NotFound) (parseHash route location)


route =
    oneOf
        [ map (Protected MyPositions) (s "")
        , map (Protected << StockPosition) (s "stock-position" </> int)
        , map (Protected StockList) (s "stocks")
        , map (Public SignIn) (s "sign-in")
        , map (Public SignUp) (s "sign-up")
        ]
