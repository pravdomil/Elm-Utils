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


apply : (a -> ( model, Cmd msg )) -> ( a, Cmd msg ) -> ( model, Cmd msg )
apply fn ( a, cmd ) =
    let
        ( model, nextCmd ) =
            fn a
    in
    ( model
    , Cmd.batch [ nextCmd, cmd ]
    )


noOperation : model -> ( model, Cmd msg )
noOperation model =
    ( model
    , Cmd.none
    )
