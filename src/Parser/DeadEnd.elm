module Parser.DeadEnd exposing (..)

import Parser as P


{-| See <https://github.com/elm/parser/pull/16>.
-}
toString : List P.DeadEnd -> String
toString a =
    a |> List.map toStringHelper |> String.join "\n"


toStringHelper : P.DeadEnd -> String
toStringHelper a =
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
        ++ " at row "
        ++ String.fromInt a.row
        ++ ", col "
        ++ String.fromInt a.col
        ++ "."
