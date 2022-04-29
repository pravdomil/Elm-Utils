module Dict.Any.Codec exposing (..)

import Codec
import Dict.Any


dict : (k -> comparable) -> Codec.Codec k -> Codec.Codec v -> Codec.Codec (Dict.Any.Dict k v)
dict toComparable a b =
    Codec.list (Codec.tuple a b)
        |> Codec.map
            (Dict.Any.fromList toComparable)
            Dict.Any.toList
