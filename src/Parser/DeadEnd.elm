module Parser.DeadEnd exposing (..)

import Parser as P


toString : P.DeadEnd -> String
toString a =
    (case a.problem of
        P.Expecting b ->
            "Expecting \"" ++ b ++ "\""

        P.ExpectingInt ->
            "Expecting integer"

        P.ExpectingHex ->
            "Expecting hex"

        P.ExpectingOctal ->
            "Expecting octal"

        P.ExpectingBinary ->
            "Expecting binary"

        P.ExpectingFloat ->
            "Expecting float"

        P.ExpectingNumber ->
            "Expecting number"

        P.ExpectingVariable ->
            "Expecting variable"

        P.ExpectingSymbol b ->
            "Expecting symbol \"" ++ b ++ "\""

        P.ExpectingKeyword b ->
            "Expecting keyword \"" ++ b ++ "\""

        P.ExpectingEnd ->
            "Expecting end"

        P.UnexpectedChar ->
            "Unexpected character"

        P.Problem b ->
            "Problem " ++ b

        P.BadRepeat ->
            "Bad repeat"
    )
        ++ " at "
        ++ String.fromInt a.row
        ++ ":"
        ++ String.fromInt a.col
        ++ "."


{-| See <https://github.com/elm/parser/pull/16>.
-}
listToString : List P.DeadEnd -> String
listToString a =
    a |> List.map toString |> String.join "\n"
