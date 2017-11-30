module Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

type alias AuthResponse =
    { token: String
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

authenticate email password =
    Http.post (apiUrl "/auth") (Http.jsonBody (authEncoder email password)) authDecoder  