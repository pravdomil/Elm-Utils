module Url.Extra exposing (..)

import Codec
import Url


{-| <https://developer.mozilla.org/en-US/docs/Glossary/Origin>
-}
toOrigin : Url.Url -> String
toOrigin a =
    Url.toString
        { a
            | path = ""
            , query = Nothing
            , fragment = Nothing
        }


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
