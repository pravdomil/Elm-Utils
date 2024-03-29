module Codec exposing
    ( Codec, build
    , encoder, encodeToString, encodeToValue
    , decoder, decodeString, decodeValue
    , unit, tuple, tuple3
    , Record, record, field, maybeField, buildRecord
    , Custom, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , int, float, char, string, list, array, dict, set
    , bool, maybe, result
    , map, andThen, oneOf, lazy, succeed, fail, value, versioned
    )

{-| A `Codec` contains a JSON encoder and decoder.
Fork of [elm-codec](https://github.com/miniBill/elm-codec).

@docs Codec, build


# Encode

@docs encoder, encodeToString, encodeToValue


# Decode

@docs decoder, decodeString, decodeValue


# Tuples

@docs unit, tuple, tuple3


# Records

@docs Record, record, field, maybeField, buildRecord


# Custom Types

@docs Custom, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Common Coders

@docs int, float, char, string, list, array, dict, set


# More Common Coders

@docs bool, maybe, result


# Helper Functions

@docs map, andThen, oneOf, lazy, succeed, fail, value, versioned

-}

import Array
import Dict
import Json.Decode
import Json.Encode
import Set


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a
    = Codec (a -> Json.Decode.Value) (Json.Decode.Decoder a)


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> Codec a
build =
    Codec



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> Json.Decode.Value
encoder (Codec a _) =
    a


{-| Convert a value into a prettified JSON string. The first argument specifies
the amount of indentation in the result string.
-}
encodeToString : Codec a -> a -> String
encodeToString a =
    encoder a >> Json.Encode.encode 0


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Json.Decode.Value
encodeToValue a =
    encoder a



-- DECODE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Json.Decode.Decoder a
decoder (Codec _ a) =
    a


{-| Parse the given string into a JSON value and then run the `Codec` on it.
This will fail if the string is not well-formed JSON or if the `Codec`
fails for some reason.
-}
decodeString : Codec a -> String -> Result Json.Decode.Error a
decodeString a =
    Json.Decode.decodeString (decoder a)


{-| Run a `Codec` to decode some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Codec a -> Json.Decode.Value -> Result Json.Decode.Error a
decodeValue a =
    Json.Decode.decodeValue (decoder a)



-- TUPLES


{-| `Codec` for unit type.
-}
unit : Codec ()
unit =
    succeed ()


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple a b =
    Codec
        (\( x1, x2 ) ->
            Json.Encode.list
                identity
                [ encoder a x1
                , encoder b x2
                ]
        )
        (Json.Decode.map2
            Tuple.pair
            (Json.Decode.index 0 (decoder a))
            (Json.Decode.index 1 (decoder b))
        )


{-| `Codec` between a JSON array of length 3 and an Elm tuple3.
-}
tuple3 : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
tuple3 a b c =
    Codec
        (\( x1, x2, x3 ) ->
            Json.Encode.list
                identity
                [ encoder a x1
                , encoder b x2
                , encoder c x3
                ]
        )
        (Json.Decode.map3
            (\x1 x2 x3 -> ( x1, x2, x3 ))
            (Json.Decode.index 0 (decoder a))
            (Json.Decode.index 1 (decoder b))
            (Json.Decode.index 2 (decoder c))
        )



-- RECORDS


{-| A partially built `Codec` for an record.
-}
type Record a b
    = Record
        { encoder : a -> List Json.Decode.Value
        , decoder : Json.Decode.Decoder b
        , index : Int
        }


{-| Start creating a `Codec` for an record. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an record.

Example with constructor:

    type alias Point =
        { x : Float
        , y : Float
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.record Point
            |> Codec.field "x" .x Codec.float
            |> Codec.field "y" .y Codec.float
            |> Codec.buildRecord

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.record (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildRecord

-}
record : b -> Record a b
record a =
    Record
        { encoder = \_ -> []
        , decoder = Json.Decode.succeed a
        , index = 0
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : (a -> c) -> Codec c -> Record a (c -> b) -> Record a b
field getter codec (Record a) =
    Record
        { encoder = \x -> encoder codec (getter x) :: a.encoder x
        , decoder = Json.Decode.map2 (\f x -> f x) a.decoder (Json.Decode.index a.index (decoder codec))
        , index = a.index + 1
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`.
If the optional field's value is `Nothing` then the resulting record will not contain that field.

-}
maybeField : (a -> c) -> Codec c -> (() -> c) -> Record a (c -> b) -> Record a b
maybeField getter codec default (Record a) =
    Record
        { encoder = \x -> encoder codec (getter x) :: a.encoder x
        , decoder =
            Json.Decode.oneOf
                [ Json.Decode.index a.index Json.Decode.value |> Json.Decode.map Just
                , Json.Decode.succeed Nothing
                ]
                |> Json.Decode.andThen
                    (\x ->
                        case x of
                            Just _ ->
                                Json.Decode.index a.index (decoder codec)

                            Nothing ->
                                Json.Decode.succeed (default ())
                    )
                |> Json.Decode.map2 (\f x -> f x) a.decoder
        , index = a.index + 1
        }


{-| Create a `Codec` from a fully specified `Record`.
-}
buildRecord : Record a a -> Codec a
buildRecord (Record a) =
    Codec
        (\x -> Json.Encode.list identity (List.reverse (a.encoder x)))
        a.decoder



-- CUSTOM TYPES


{-| A partially built `Codec` for a custom type.
-}
type Custom x a
    = Custom
        { encoder : x
        , decoder : Dict.Dict Int (Json.Decode.Decoder a)
        , index : Int
        }


{-| Starts building a `Codec` for a custom type.

You need to pass a pattern matching function, built like this:

    type Semaphore
        = Red Int String
        | Yellow
        | Green Float

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\red yellow green value ->
                case value of
                    Red i s ->
                        red i s

                    Yellow ->
                        yellow

                    Green f ->
                        green f
            )
            |> Codec.variant2 "Red" Red Codec.int Codec.string
            |> Codec.variant0 "Yellow" Yellow
            |> Codec.variant1 "Green" Green Codec.float
            |> Codec.buildCustom

-}
custom : x -> Custom x a
custom a =
    Custom
        { encoder = a
        , decoder = Dict.empty
        , index = 0
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    a
    -> Custom (Json.Decode.Value -> x) a
    -> Custom x a
variant0 fn (Custom a) =
    let
        encoder_ : Json.Encode.Value
        encoder_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                ]

        decoder_ : Json.Decode.Decoder a
        decoder_ =
            Json.Decode.succeed fn
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> value)
    -> Codec a
    -> Custom ((a -> Json.Decode.Value) -> x) value
    -> Custom x value
variant1 fn codecA (Custom a) =
    let
        encoder_ : a -> Json.Encode.Value
        encoder_ a_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map fn
                (Json.Decode.index 1 (decoder codecA))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    (a -> b -> value)
    -> Codec a
    -> Codec b
    -> Custom ((a -> b -> Json.Decode.Value) -> x) value
    -> Custom x value
variant2 fn codecA codecB (Custom a) =
    let
        encoder_ : a -> b -> Json.Encode.Value
        encoder_ a_ b_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map2 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    (a -> b -> c -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Custom ((a -> b -> c -> Json.Decode.Value) -> x) value
    -> Custom x value
variant3 fn codecA codecB codecC (Custom a) =
    let
        encoder_ : a -> b -> c -> Json.Encode.Value
        encoder_ a_ b_ c_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map3 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    (a -> b -> c -> d -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Custom ((a -> b -> c -> d -> Json.Decode.Value) -> x) value
    -> Custom x value
variant4 fn codecA codecB codecC codecD (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map4 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    (a -> b -> c -> d -> e -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Custom ((a -> b -> c -> d -> e -> Json.Decode.Value) -> x) value
    -> Custom x value
variant5 fn codecA codecB codecC codecD codecE (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map5 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Custom ((a -> b -> c -> d -> e -> f -> Json.Decode.Value) -> x) value
    -> Custom x value
variant6 fn codecA codecB codecC codecD codecE codecF (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map6 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Custom ((a -> b -> c -> d -> e -> f -> g -> Json.Decode.Value) -> x) value
    -> Custom x value
variant7 fn codecA codecB codecC codecD codecE codecF codecG (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> g -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ g_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                , encoder codecG g_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map7 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
                (Json.Decode.index 7 (decoder codecG))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Codec h
    -> Custom ((a -> b -> c -> d -> e -> f -> g -> h -> Json.Decode.Value) -> x) value
    -> Custom x value
variant8 fn codecA codecB codecC codecD codecE codecF codecG codecH (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> g -> h -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ g_ h_ =
            Json.Encode.list identity
                [ Json.Encode.int a.index
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                , encoder codecG g_
                , encoder codecH h_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map8 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
                (Json.Decode.index 7 (decoder codecG))
                (Json.Decode.index 8 (decoder codecH))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert a.index decoder_ a.decoder
        , index = a.index + 1
        }


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : Custom (a -> Json.Decode.Value) a -> Codec a
buildCustom (Custom a) =
    Codec
        (\x -> a.encoder x)
        (Json.Decode.index 0 Json.Decode.int
            |> Json.Decode.andThen
                (\x ->
                    case Dict.get x a.decoder of
                        Nothing ->
                            Json.Decode.fail ("Unknown variant " ++ String.fromInt x ++ ".")

                        Just x2 ->
                            x2
                )
        )



-- OPAQUE CUSTOM TYPES


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    build Json.Encode.int Json.Decode.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    build Json.Encode.float Json.Decode.float


{-| `Codec` between a JSON string of length 1 and an Elm `Char`
-}
char : Codec Char
char =
    build
        (String.fromChar >> Json.Encode.string)
        (Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case String.uncons x of
                        Just ( h, "" ) ->
                            Json.Decode.succeed h

                        _ ->
                            Json.Decode.fail "Expecting character."
                )
        )


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    build Json.Encode.string Json.Decode.string


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list a =
    build (Json.Encode.list (encoder a)) (Json.Decode.list (decoder a))


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array.Array a)
array a =
    build (Json.Encode.array (encoder a)) (Json.Decode.array (decoder a))


{-| `Codec` between a JSON array and an Elm `Dict`.
-}
dict : Codec comparable -> Codec v -> Codec (Dict.Dict comparable v)
dict k v =
    map Dict.toList Dict.fromList (list (tuple k v))


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set.Set comparable)
set a =
    map Set.toList Set.fromList (list a)



-- COMMON CUSTOM TYPES


{-| `Codec` for `Bool` values.
-}
bool : Codec Bool
bool =
    custom
        (\fn1 fn2 x ->
            case x of
                True ->
                    fn1

                False ->
                    fn2
        )
        |> variant0 True
        |> variant0 False
        |> buildCustom


{-| `Codec` for `Maybe` values.
-}
maybe : Codec a -> Codec (Maybe a)
maybe a =
    custom
        (\fn1 fn2 x ->
            case x of
                Just x1 ->
                    fn1 x1

                Nothing ->
                    fn2
        )
        |> variant1 Just a
        |> variant0 Nothing
        |> buildCustom


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec a -> Codec (Result error a)
result error a =
    custom
        (\fn1 fn2 x ->
            case x of
                Ok x1 ->
                    fn1 x1

                Err x1 ->
                    fn2 x1
        )
        |> variant1 Ok a
        |> variant1 Err error
        |> buildCustom



-- HELPER FUNCTIONS


{-| Transform a `Codec`.
-}
map : (b -> a) -> (a -> b) -> Codec a -> Codec b
map bToA aToB a =
    Codec
        (\x -> encoder a (bToA x))
        (Json.Decode.map aToB (decoder a))


{-| Create codecs that depend on previous results.
-}
andThen : (b -> a) -> (a -> Codec b) -> Codec a -> Codec b
andThen bToA aToB a =
    Codec
        (encoder a << bToA)
        (decoder a |> Json.Decode.andThen (aToB >> decoder))


{-| Try a set of decoders (in order).
The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.

This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
and the old ones (eventually `map`ped) as a fallback list to use while decoding.

-}
oneOf : List (Codec a) -> Codec a -> Codec a
oneOf codecs a =
    Codec
        (encoder a)
        (Json.Decode.oneOf (decoder a :: List.map decoder codecs))


{-| This is useful for recursive structures that are not easily modeled with `recursive`.
Have a look at the Json.Decode docs for examples.
-}
lazy : (() -> Codec a) -> Codec a
lazy fn =
    Codec
        (\x -> encoder (fn ()) x)
        (Json.Decode.lazy (\_ -> decoder (fn ())))


{-| Create a `Codec` that always decodes as the same value.
-}
succeed : a -> Codec a
succeed a =
    Codec
        (\_ -> Json.Encode.list identity [])
        (Json.Decode.succeed a)


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.
-}
fail : String -> Codec a
fail a =
    Codec
        (\_ -> encodeToValue unit ())
        (Json.Decode.fail a)


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Json.Decode.Value
value =
    Codec
        identity
        Json.Decode.value


{-| First attempt.
-}
versioned : String -> Codec a -> Codec a
versioned version codec =
    Codec
        (\x ->
            Json.Encode.list
                identity
                [ Json.Encode.string version
                , encoder codec x
                ]
        )
        (Json.Decode.index 0 Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    if x == version then
                        Json.Decode.index 1 (decoder codec)

                    else
                        Json.Decode.fail "Unknown version."
                )
        )
