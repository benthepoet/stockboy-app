module Request exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

apiUrl path =
    "https://api.stockboy.us" + path

authDecoder = 
    Decode.decodeString (Decode.field "token" Decode.string)

authEncoder email password = 
    Encode.object
        [ ("email", Encode.string email)
        , ("password", Encode.string password)
        ]

authenticate email password =
    Http.post (apiUrl "/auth") (Http.jsonBody (authEncoder email password)) authDecoder  