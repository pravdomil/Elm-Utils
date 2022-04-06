module Url.QueryString exposing (..)

import Dict
import Url


type alias QueryString =
    Dict.Dict String String


fromString : String -> QueryString
fromString a =
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
    case a |> String.split "&" of
        [] ->
            Dict.empty

        b ->
            b |> List.filterMap parseParam |> Dict.fromList


fromUrl : Url.Url -> QueryString
fromUrl a =
    a.query
        |> Maybe.map fromString
        |> Maybe.withDefault Dict.empty


toString : QueryString -> String
toString a =
    a
        |> Dict.toList
        |> toStringFromList


toStringFromList : List ( String, String ) -> String
toStringFromList a =
    a
        |> List.map (\( k, v ) -> Url.percentEncode k ++ "=" ++ Url.percentEncode v)
        |> String.join "&"
