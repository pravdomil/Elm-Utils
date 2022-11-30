module Platform.Extra exposing (..)


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            fn model
    in
    ( nextModel
    , Cmd.batch [ nextCmd, cmd ]
    )


apply : ( a, Cmd msg ) -> ( a -> model, Cmd msg ) -> ( model, Cmd msg )
apply ( a, nextCmd ) ( fn, cmd ) =
    ( fn a
    , Cmd.batch [ nextCmd, cmd ]
    )


noOperation : model -> ( model, Cmd msg )
noOperation model =
    ( model
    , Cmd.none
    )
