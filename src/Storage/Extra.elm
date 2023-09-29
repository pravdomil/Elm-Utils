module Storage.Extra exposing (..)

import Codec
import JavaScript
import Storage
import Task


get : Codec.Codec a -> Storage.Storage -> Task.Task JavaScript.Error (Maybe a)
get codec a =
    Storage.get a
        |> Task.andThen
            (\x ->
                case x of
                    Just x2 ->
                        case Codec.decodeString codec x2 of
                            Ok x3 ->
                                Task.succeed (Just x3)

                            Err x3 ->
                                Task.fail (JavaScript.DecodeError x3)

                    Nothing ->
                        Task.succeed Nothing
            )


set : Codec.Codec a -> Storage.Storage -> Maybe a -> Task.Task JavaScript.Error ()
set codec storage a =
    Storage.set storage (Maybe.map (Codec.encodeToString codec) a)
