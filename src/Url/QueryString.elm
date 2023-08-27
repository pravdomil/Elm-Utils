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
            case String.split "=" b of
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
    case String.split "&" a of
        [] ->
            Dict.empty

        "" :: [] ->
            Dict.empty

        b ->
            Dict.fromList (List.filterMap parseParam b)


fromUrl : Url.Url -> QueryString
fromUrl a =
    Maybe.withDefault Dict.empty (Maybe.map fromString a.query)


toString : QueryString -> String
toString a =
    entriesToString (Dict.toList a)


entriesToString : List ( String, String ) -> String
entriesToString a =
    String.join "&" (List.map entryToString a)


maybeEntriesToString : List ( String, Maybe String ) -> String
maybeEntriesToString a =
    String.join "&" (List.filterMap (\( k, v ) -> Maybe.map (\x -> entryToString ( k, x )) v) a)


entryToString : ( String, String ) -> String
entryToString ( k, v ) =
    Url.percentEncode k ++ "=" ++ Url.percentEncode v
