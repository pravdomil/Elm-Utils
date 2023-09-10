module Http.Codec exposing (..)

import Codec
import Http


errorCodec : Codec.Codec Http.Error
errorCodec =
    Codec.custom
        (\fn1 fn2 fn3 fn4 fn5 x ->
            case x of
                Http.BadUrl x1 ->
                    fn1 x1

                Http.Timeout ->
                    fn2

                Http.NetworkError ->
                    fn3

                Http.BadStatus x1 ->
                    fn4 x1

                Http.BadBody x1 ->
                    fn5 x1
        )
        |> Codec.variant1 Http.BadUrl Codec.string
        |> Codec.variant0 Http.Timeout
        |> Codec.variant0 Http.NetworkError
        |> Codec.variant1 Http.BadStatus Codec.int
        |> Codec.variant1 Http.BadBody Codec.string
        |> Codec.buildCustom
