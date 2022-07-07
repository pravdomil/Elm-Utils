module Codec.Extra exposing (..)

import Codec


maybe : Codec.Codec a -> Codec.Codec (Maybe a)
maybe a =
    Codec.custom
        (\fn1 fn2 x ->
            case x of
                Just x2 ->
                    fn1 x2

                Nothing ->
                    fn2
        )
        |> Codec.variant1 "Just" Just a
        |> Codec.variant0 "Nothing" Nothing
        |> Codec.buildCustom
