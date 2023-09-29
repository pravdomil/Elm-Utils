module Storage.Extra exposing (..)

import Codec
import JavaScript
import Json.Decode
import Storage
import Task
import Task.Extra


get : Codec.Codec a -> Storage.Storage -> Task.Task JavaScript.Error (Maybe a)
get codec a =
    Storage.get a
        |> Task.andThen
            (\x ->
                Task.Extra.fromResult
                    (Result.mapError JavaScript.DecodeError
                        (decodeHelper codec x)
                    )
            )


set : Codec.Codec a -> Storage.Storage -> Maybe a -> Task.Task JavaScript.Error ()
set codec storage a =
    Storage.set storage (Maybe.map (Codec.encodeToString codec) a)


onChange : msg -> (Result Json.Decode.Error (Maybe a) -> msg) -> Codec.Codec a -> Storage.Storage -> Sub msg
onChange noOperation toMsg codec storage =
    Storage.onChange
        noOperation
        (\x -> toMsg (decodeHelper codec x))
        storage



--


decodeHelper : Codec.Codec a -> Maybe String -> Result Json.Decode.Error (Maybe a)
decodeHelper codec a =
    case a of
        Just b ->
            case Codec.decodeString codec b of
                Ok c ->
                    Ok (Just c)

                Err c ->
                    Err c

        Nothing ->
            Ok Nothing
