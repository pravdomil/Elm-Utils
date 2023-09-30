module Basics.Extra exposing (..)

{-| -}


{-| Patched by Elm FFI.
-}
referenceEqual : a -> a -> Bool
referenceEqual a b =
    False


{-| Patched by Elm FFI.
-}
memorize : (a -> b) -> (a -> b)
memorize fn =
    \x -> fn x
