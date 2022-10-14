module Parser.DeadEnd exposing (..)

import Parser


toString : Parser.DeadEnd -> String
toString a =
    (case a.problem of
        Parser.Expecting b ->
            "Expecting \"" ++ b ++ "\""

        Parser.ExpectingInt ->
            "Expecting integer"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol b ->
            "Expecting symbol \"" ++ b ++ "\""

        Parser.ExpectingKeyword b ->
            "Expecting keyword \"" ++ b ++ "\""

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected character"

        Parser.Problem b ->
            "Problem " ++ b

        Parser.BadRepeat ->
            "Bad repeat"
    )
        ++ " at "
        ++ String.fromInt a.row
        ++ ":"
        ++ String.fromInt a.col
        ++ "."


{-| See <https://github.com/elm/parser/pull/16>.
-}
listToString : List Parser.DeadEnd -> String
listToString a =
    a
        |> List.map toString
        |> String.join "\n"
