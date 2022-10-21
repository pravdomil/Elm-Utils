module Email.Codec exposing (..)

import Codec
import Email


email : Codec.Codec Email.Email
email =
    Codec.string
        |> Codec.andThen
            Email.toString
            (\x ->
                case Email.fromString x of
                    Just x2 ->
                        Codec.succeed x2

                    Nothing ->
                        Codec.fail "Cannot decode e-mail."
            )
