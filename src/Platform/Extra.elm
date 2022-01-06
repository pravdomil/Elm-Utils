module Platform.Extra exposing (..)

import Task


updateMultiple : (msg -> model -> ( model, Cmd msg )) -> List msg -> model -> ( model, Cmd msg )
updateMultiple updateFn msgs model =
    msgs
        |> List.foldl
            (\msg ( model_, cmds ) ->
                updateFn msg model_
                    |> Tuple.mapSecond (\v -> v :: cmds)
            )
            ( model
            , []
            )
        |> Tuple.mapSecond Cmd.batch


sendMsg : a -> Cmd a
sendMsg a =
    Task.succeed () |> Task.perform (\_ -> a)
