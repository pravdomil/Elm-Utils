module Dict.Any.Codec exposing (..)

import Codec
import Dict.Any


dict : (k -> comparable) -> Codec.Codec k -> Codec.Codec v -> Codec.Codec (Dict.Any.Dict k v)
dict toComparable a b =
    Codec.list (Codec.tuple a b)
        |> Codec.map Dict.Any.toList (Dict.Any.fromList toComparable)
