module Utils.Resolver exposing (..)

import Http
import Json.Decode as Decode


string : Http.Resolver Http.Error String
string =
    helper Ok


json : Decode.Decoder a -> Http.Resolver Http.Error a
json decoder =
    helper
        (\v ->
            v
                |> Decode.decodeString decoder
                |> Result.mapError (Decode.errorToString >> Http.BadBody)
        )



--


helper : (String -> Result Http.Error a) -> Http.Resolver Http.Error a
helper fn =
    let
        toResult : Http.Response String -> Result Http.Error a
        toResult a =
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
                    fn b
    in
    Http.stringResolver toResult
