module Maybe.Extra exposing (..)


sequence : List (Maybe a) -> Maybe (List a)
sequence a =
    List.foldr (Maybe.map2 (::)) (Just []) a


onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing fn a =
    case a of
        Just _ ->
            a

        Nothing ->
            fn ()
