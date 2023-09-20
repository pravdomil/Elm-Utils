module String.Extra2 exposing (..)


leftUntilLast : String -> String -> String
leftUntilLast needle a =
    case List.head (List.reverse (String.indexes needle a)) of
        Just b ->
            String.left (b + 1) a

        Nothing ->
            a


dropLeftUntilLast : String -> String -> String
dropLeftUntilLast needle a =
    case List.head (List.reverse (String.indexes needle a)) of
        Just b ->
            String.dropLeft (b + 1) a

        Nothing ->
            a
