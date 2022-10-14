module Platform.Extra exposing (..)


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( nextModel, cmd2 ) =
            fn model
    in
    ( nextModel
    , Cmd.batch [ cmd2, cmd ]
    )
