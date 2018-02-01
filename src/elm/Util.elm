module Util exposing (..)

formatCurrency value =
    "$" ++ (toFixed 2 value)

toFixed places value =
    let
        parts =
            String.split "." (toString <| (toFloat (round <| value * 10 ^ places) / 10 ^ places))
    in
        let
            head =
                List.head parts |> Maybe.withDefault "0"

            tail =
                List.tail parts |> Maybe.withDefault []
        in
            String.join "." (head :: List.map (String.padRight (round places) '0') tail)
