module String.Extra2 exposing (..)


splitByLast : String -> String -> Maybe ( String, String, String )
splitByLast needle a =
    case List.head (List.reverse (String.indexes needle a)) of
        Just b ->
            Just
                ( String.left b a
                , needle
                , String.dropLeft (b + 1) a
                )

        Nothing ->
            Nothing
