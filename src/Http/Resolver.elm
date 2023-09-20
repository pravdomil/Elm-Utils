module Http.Resolver exposing (..)

import Bytes
import Http
import Json.Decode as Decode


string : Http.Resolver Http.Error String
string =
    Http.stringResolver (custom Ok)


json : Decode.Decoder a -> Http.Resolver Http.Error a
json decoder =
    Http.stringResolver
        (custom
            (\x ->
                Decode.decodeString decoder x
                    |> Result.mapError Decode.errorToString
            )
        )



--


customString : (String -> Result String a) -> Http.Resolver Http.Error a
customString a =
    Http.stringResolver (custom a)


customBytes : (Bytes.Bytes -> Result String a) -> Http.Resolver Http.Error a
customBytes a =
    Http.bytesResolver (custom a)



--


custom : (body -> Result String a) -> Http.Response body -> Result Http.Error a
custom fn a =
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
