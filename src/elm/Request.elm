module Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Auth =
    { token : String
    }


type alias Order =
    { units : Int
    }


type alias Position =
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


apiUrl =
    (++) "https://api.stockboy.us/"


authDecoder =
    Decode.map Auth <| Decode.field "token" Decode.string


authEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


orderEncoder units =
    Encode.object
        [ ( "units", Encode.int units )
        ]


orderDecoder =
    Decode.map Order
        (Decode.field "units" Decode.int)


positionDecoder =
    Decode.map4 Position
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
    Http.post (apiUrl "auth") (Http.jsonBody <| authEncoder email password) authDecoder


createStockOrder token id units =
    post ("stocks/" ++ (toString id) ++ "/orders") token orderDecoder <| orderEncoder units


getPositions token =
    get "positions" token <| Decode.list positionDecoder


getStock token id =
    get ("stocks/" ++ (toString id)) token stockDecoder


getStocks token search =
    get ("stocks?search=" ++ search) token <| Decode.list stockDecoder


getUser token =
    get "users/me" token userDecoder


signUp email password =
    Http.post (apiUrl "auth/signup") (Http.jsonBody <| authEncoder email password) authDecoder


get path token decoder =
    Http.request
        { method = "GET"
        , url = apiUrl path
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


post path token decoder body =
    Http.request
        { method = "POST"
        , url = apiUrl path
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
