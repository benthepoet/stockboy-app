module Util exposing (toFixed)

import Array

toFixed places value =
    let
        parts =
            String.split "." (toString <| (toFloat (round <| value * 10 ^ places) / 10 ^ places))
    in
        let
            head = Maybe.withDefault "0" (List.head parts)
            tail = Maybe.withDefault [""] (List.tail parts) 
        in
            String.join "." (head :: List.map (String.padRight (round places) '0') tail)