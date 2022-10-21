module Url.Codec exposing (..)

import Codec
import Url


url : Codec.Codec Url.Url
url =
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
