module Json.Decode.Extra2 exposing (..)

import Json.Decode
import Json.Decode.Extra2


apply : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
apply decoder a =
    Json.Decode.map2 (\fn x -> fn x) a decoder


maybeField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
maybeField a decoder =
    Json.Decode.oneOf
        [ Json.Decode.field a (Json.Decode.map Just Json.Decode.value)
        , Json.Decode.succeed Nothing
        ]
        |> Json.Decode.andThen
            (\x ->
                case x of
                    Just _ ->
                        Json.Decode.field a (Json.Decode.map Just decoder)

                    Nothing ->
                        Json.Decode.succeed Nothing
            )
