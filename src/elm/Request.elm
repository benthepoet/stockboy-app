module Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias AuthResponse =
    { token : String
    }


type alias PositionResponse =
    { averagePrice : Float
    , profitRatio : Float
    , totalUnits : Int
    , stock : Stock
    }


type alias Stock =
    { id : Int
    , symbol : String
    , name : String
    , lastPrice : Float
    }


type alias User =
    { balance : Float
    }


apiUrl path =
    "https://api.stockboy.us/" ++ path


authDecoder =
    Decode.map AuthResponse (Decode.field "token" Decode.string)


authEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


positionDecoder =
    Decode.map4 PositionResponse
        (Decode.field "average_price" Decode.float)
        (Decode.field "profit_ratio" Decode.float)
        (Decode.field "total_units" Decode.int)
        (Decode.field "stock" stockDecoder)


stockDecoder =
    Decode.map4 Stock
        (Decode.field "id" Decode.int)
        (Decode.field "symbol" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "last_price" (Decode.oneOf [ Decode.float, Decode.null 0 ]))


userDecoder =
    Decode.map User
        (Decode.field "balance" Decode.float)


authenticate email password =
    Http.post (apiUrl "auth") (Http.jsonBody (authEncoder email password)) authDecoder


getPositions token =
    get "positions" token (Decode.list positionDecoder)


getStock token id =
    get ("stocks/" ++ (toString id)) token stockDecoder


getStocks token search =
    get ("stocks?search=" ++ search) token (Decode.list stockDecoder)


getUser token =
    get "users/me" token userDecoder


get path token decoder =
    Http.request
        { method = "GET"
        , url = apiUrl path
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
