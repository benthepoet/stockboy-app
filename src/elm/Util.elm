module Util exposing (toFixed)

import Array

extract index parts = 
    case Array.get index parts of 
        Just part -> part
        Nothing -> "0"

toFixed places value =
    let
        parts =
            Array.fromList <| String.split "." (toString <| (toFloat (round <| value * 10 ^ places) / 10 ^ places))
    
    in
        let
            head = extract 0 parts 
            tail = extract 1 parts
        in
            String.join "." [head, (String.padRight (round places) '0' tail)]