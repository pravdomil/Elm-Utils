module Time.Codec exposing (..)

import Codec
import Time


posix : Codec.Codec Time.Posix
posix =
    Codec.int |> Codec.map Time.millisToPosix Time.posixToMillis
