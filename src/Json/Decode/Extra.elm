module Json.Decode.Extra exposing (..)

import Json.Decode


maybeField : String -> Json.Decode.Decoder (Maybe a) -> Json.Decode.Decoder (Maybe a)
maybeField name a =
    Json.Decode.oneOf
        [ Json.Decode.field name Json.Decode.value |> Json.Decode.map Just
        , Json.Decode.succeed Nothing
        ]
        |> Json.Decode.andThen
            (\v ->
                case v of
                    Just _ ->
                        Json.Decode.field name a

                    Nothing ->
                        Json.Decode.succeed Nothing
            )
