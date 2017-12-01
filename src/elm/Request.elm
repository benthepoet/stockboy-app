module Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

type alias AuthResponse =
    { token: String
    }
    
type alias PositionResponse =
    { averagePrice: Float
    , profitRatio: Float
    , totalUnits: Int
    }

apiUrl path =
    "https://api.stockboy.us" ++ path

authDecoder = 
    Decode.map AuthResponse (Decode.field "token" Decode.string)

authEncoder email password = 
    Encode.object
        [ ("email", Encode.string email)
        , ("password", Encode.string password)
        ]
        
positionDecoder =
    Decode.map3 PositionResponse 
        (Decode.field "average_price" Decode.float)
        (Decode.field "profit_ratio" Decode.float)
        (Decode.field "total_units" Decode.int)

authenticate email password =
    Http.post (apiUrl "/auth") (Http.jsonBody (authEncoder email password)) authDecoder  
    
getPositions token = 
    get "/positions" token (Decode.list positionDecoder)
        
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