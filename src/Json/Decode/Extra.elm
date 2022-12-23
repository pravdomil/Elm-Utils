module Json.Decode.Extra exposing (..)

import Json.Decode


apply : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
apply decoder a =
    Json.Decode.map2 (\fn x -> fn x) a decoder
