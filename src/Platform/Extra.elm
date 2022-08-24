module Platform.Extra exposing (..)

import Task


applyInit : ( arg, Cmd msg ) -> ( arg -> model, Cmd msg ) -> ( model, Cmd msg )
applyInit ( arg, argCmd ) ( toModel, cmd ) =
    ( toModel arg
    , Cmd.batch [ argCmd, cmd ]
    )


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


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( nextModel, cmd2 ) =
            fn model
    in
    ( nextModel
    , Cmd.batch [ cmd2, cmd ]
    )


sendMsg : a -> Cmd a
sendMsg a =
    Task.succeed () |> Task.perform (\_ -> a)
