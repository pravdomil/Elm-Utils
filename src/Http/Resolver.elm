module Http.Resolver exposing (..)

import Bytes
import Codec
import Http
import Json.Decode as Decode


customString : (String -> Result String a) -> Http.Resolver Http.Error a
customString a =
    Http.stringResolver (helper a)


customBytes : (Bytes.Bytes -> Result String a) -> Http.Resolver Http.Error a
customBytes a =
    Http.bytesResolver (helper a)


json : Decode.Decoder a -> Http.Resolver Http.Error a
json a =
    customString (\x -> Result.mapError Decode.errorToString (Decode.decodeString a x))


codec : Codec.Codec a -> Http.Resolver Http.Error a
codec a =
    customString (\x -> Result.mapError Decode.errorToString (Codec.decodeString a x))



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
