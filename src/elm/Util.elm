module Util exposing (..)

formatCurrency =
    (++) "$" << (toFixed 2)

toFixed places value = 
    let
        stringValue = String.padLeft (places + 1) '0' << toString << round <| (*) value <| (^) 10 <| toFloat places
        valueLength = String.length stringValue
    in
        String.left (valueLength - places) stringValue ++ "." ++ String.right places stringValue