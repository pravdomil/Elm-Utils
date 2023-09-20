module Http.Resolver exposing (..)

import Bytes
import Codec
import Dict
import Http
import Json.Decode


string : (String -> Result String a) -> Http.Resolver Http.Error a
string a =
    Http.stringResolver (helper a)


json : Json.Decode.Decoder a -> Http.Resolver Http.Error a
json a =
    string (\x -> Result.mapError Json.Decode.errorToString (Json.Decode.decodeString a x))


codec : Codec.Codec a -> Http.Resolver Http.Error a
codec codec_ =
    let
        decode : Http.Metadata -> String -> Result Http.Error a
        decode meta a =
            case Dict.get "content-type" meta.headers of
                Just b ->
                    case b of
                        "application/json" ->
                            case Codec.decodeString codec_ a of
                                Ok c ->
                                    Ok c

                                Err c ->
                                    Err (Http.BadBody (Json.Decode.errorToString c))

                        _ ->
                            Err (Http.BadBody ("Bad content type " ++ b ++ ", status is " ++ String.fromInt meta.statusCode ++ "."))

                Nothing ->
                    Err (Http.BadBody "No content type.")
    in
    Http.stringResolver (helper2 decode)


bytes : (Bytes.Bytes -> Result String a) -> Http.Resolver Http.Error a
bytes a =
    Http.bytesResolver (helper a)



--


helper : (body -> Result String a) -> Http.Response body -> Result Http.Error a
helper fn a =
    case a of
        Http.BadUrl_ b ->
            Err (Http.BadUrl b)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ b _ ->
            Err (Http.BadStatus b.statusCode)

        Http.GoodStatus_ _ b ->
            Result.mapError Http.BadBody (fn b)


helper2 : (Http.Metadata -> body -> Result Http.Error a) -> Http.Response body -> Result Http.Error a
helper2 fn a =
    case a of
        Http.BadUrl_ b ->
            Err (Http.BadUrl b)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ b c ->
            fn b c

        Http.GoodStatus_ b c ->
            fn b c
