module Task.Extra exposing (..)

import Task


fromMaybe : x -> Maybe a -> Task.Task x a
fromMaybe x a =
    case a of
        Just b ->
            Task.succeed b

        Nothing ->
            Task.fail x


fromResult : Result x a -> Task.Task x a
fromResult a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b



--


{-| <https://github.com/elm/core/issues/1111#issuecomment-892582007>
-}
andAlwaysThen : (Result x a -> Task.Task y b) -> Task.Task x a -> Task.Task y b
andAlwaysThen toTask a =
    a
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)
        |> Task.andThen toTask



--


do2 : Task.Task x a -> Task.Task y b -> Task.Task z ( Result x a, Result y b )
do2 a b =
    andAlwaysThen
        (\x ->
            andAlwaysThen
                (\x2 ->
                    Task.succeed ( x, x2 )
                )
                b
        )
        a



--


apply : Task.Task x a -> Task.Task x (a -> b) -> Task.Task x b
apply task a =
    Task.map2 (\fn x -> fn x) a task



--


toCmd : Task.Task x a -> Cmd msg
toCmd a =
    toCmdHelper (andAlwaysThen Task.succeed a)


{-| Patched by Elm FFI.
-}
toCmdHelper : Task.Task Never a -> Cmd msg
toCmdHelper a =
    Cmd.none
