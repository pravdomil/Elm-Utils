module List.Extra exposing (..)


headAndTail : List a -> Maybe ( a, List a )
headAndTail a =
    case a of
        [] ->
            Nothing

        b :: rest ->
            Just
                ( b
                , rest
                )


find : (a -> Bool) -> List a -> Maybe a
find isOk a =
    case a of
        [] ->
            Nothing

        b :: rest ->
            if isOk b then
                Just b

            else
                find isOk rest


findPreviousCurrentNext : (a -> Bool) -> List a -> { previous : Maybe a, current : Maybe a, next : Maybe a }
findPreviousCurrentNext isCurrent a =
    let
        find_ : Maybe a -> List a -> Maybe { previous : Maybe a, current : Maybe a, next : Maybe a }
        find_ previous b =
            case b of
                [] ->
                    Nothing

                c :: rest ->
                    if isCurrent c then
                        Just
                            { previous = previous
                            , current = Just c
                            , next = rest |> List.head
                            }

                    else
                        find_ (Just c) rest
    in
    case a |> find_ Nothing of
        Just b ->
            b

        Nothing ->
            { previous = a |> List.reverse |> List.head
            , current = Nothing
            , next = a |> List.head
            }
