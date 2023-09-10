module Parser.DeadEnd exposing (..)

import Parser
import Parser.Advanced


toString : Parser.DeadEnd -> String
toString a =
    (case a.problem of
        Parser.Expecting b ->
            "Expecting " ++ b ++ "."

        Parser.ExpectingInt ->
            "Expecting integer."

        Parser.ExpectingHex ->
            "Expecting hex."

        Parser.ExpectingOctal ->
            "Expecting octal."

        Parser.ExpectingBinary ->
            "Expecting binary."

        Parser.ExpectingFloat ->
            "Expecting float."

        Parser.ExpectingNumber ->
            "Expecting number."

        Parser.ExpectingVariable ->
            "Expecting variable."

        Parser.ExpectingSymbol b ->
            "Expecting symbol " ++ b ++ "."

        Parser.ExpectingKeyword b ->
            "Expecting keyword " ++ b ++ "."

        Parser.ExpectingEnd ->
            "Expecting end."

        Parser.UnexpectedChar ->
            "Unexpected character."

        Parser.Problem b ->
            b

        Parser.BadRepeat ->
            "Bad repeat."
    )
        ++ "\n"
        ++ ("Check row " ++ String.fromInt a.row ++ " column " ++ String.fromInt a.col ++ ".")


{-| See <https://github.com/elm/parser/pull/16>.
-}
listToString : List Parser.DeadEnd -> String
listToString a =
    String.join "\n" (List.map toString a)



--


advancedToString : Parser.Advanced.DeadEnd a Parser.Problem -> String
advancedToString a =
    toString (Parser.DeadEnd a.row a.col a.problem)


advancedListToString : List (Parser.Advanced.DeadEnd a Parser.Problem) -> String
advancedListToString a =
    String.join "\n" (List.map advancedToString a)
