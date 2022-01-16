module Result.Extra exposing (..)


sequence : List (Result x a) -> Result x (List a)
sequence tasks =
    List.foldr (Result.map2 (::)) (Ok []) tasks


onErr : (x -> Result y a) -> Result x a -> Result y a
onErr fn a =
    case a of
        Ok b ->
            Ok b

        Err b ->
            fn b
