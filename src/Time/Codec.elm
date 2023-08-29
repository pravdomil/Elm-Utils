module Time.Codec exposing (..)

import Codec
import Time


posix : Codec.Codec Time.Posix
posix =
    Codec.map Time.posixToMillis Time.millisToPosix Codec.int
