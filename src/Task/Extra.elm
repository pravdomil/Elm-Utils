module Task.Extra exposing (..)

import Task


fromMaybe : x -> Maybe a -> Task.Task x a
fromMaybe x a =
    case a of
        Just b ->
            Task.succeed b

        Nothing ->
            Task.fail x



--


fromResult : Result x a -> Task.Task x a
fromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b


{-| <https://github.com/elm/core/issues/1111#issuecomment-892582007>
-}
taskAndThenWithResult : (Result x a -> Task.Task y b) -> Task.Task x a -> Task.Task y b
taskAndThenWithResult toTask a =
    a
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.andThen toTask



--


apply : Task.Task x a -> Task.Task x (a -> b) -> Task.Task x b
apply task a =
    Task.map2 (\fn v -> fn v) a task
