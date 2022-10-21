module Url.Extra exposing (..)

import Codec
import Url


codec : Codec.Codec Url.Url
codec =
    Codec.string
        |> Codec.andThen
            Url.toString
            (\x ->
                case Url.fromString x of
                    Just x2 ->
                        Codec.succeed x2

                    Nothing ->
                        Codec.fail "Cannot decode URL."
            )
