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

        "" :: [] ->
            Dict.empty

        b ->
            b |> List.filterMap parseParam |> Dict.fromList


fromUrl : Url.Url -> QueryString
fromUrl a =
    Maybe.map fromString a.query
        |> Maybe.withDefault Dict.empty


toString : QueryString -> String
toString a =
    Dict.toList a
        |> entriesToString


entriesToString : List ( String, String ) -> String
entriesToString a =
    List.map entryToString a |> String.join "&"


maybeEntriesToString : List ( String, Maybe String ) -> String
maybeEntriesToString a =
    List.filterMap (\( k, v ) -> v |> Maybe.map (\x -> Url.percentEncode k ++ "=" ++ Url.percentEncode x)) a
        |> String.join "&"


entryToString : ( String, String ) -> String
entryToString ( k, v ) =
    Url.percentEncode k ++ "=" ++ Url.percentEncode v
