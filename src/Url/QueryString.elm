module Url.QueryString exposing (..)

import Dict
import Url
import Url.Builder as Builder


type alias QueryString =
    Dict.Dict String String


parse : Url.Url -> QueryString
parse a =
    let
        parseParam : String -> Maybe ( String, String )
        parseParam b =
            case b |> String.split "=" of
                c :: [] ->
                    Maybe.map2 Tuple.pair
                        (Url.percentDecode c)
                        (Just "")

                c :: d :: [] ->
                    Maybe.map2 Tuple.pair
                        (Url.percentDecode c)
                        (Url.percentDecode d)

                _ ->
                    Nothing
    in
    case a.query |> Maybe.withDefault "" of
        "" ->
            Dict.empty

        b ->
            b |> String.split "&" |> List.filterMap parseParam |> Dict.fromList


build : QueryString -> String
build a =
    a
        |> Dict.toList
        |> List.map (\( k, v ) -> Builder.string k v)
        |> Builder.relative [ "." ]
