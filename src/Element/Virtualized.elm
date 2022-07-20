module Element.Virtualized exposing (ScrollOffset(..), column)

import Element exposing (..)
import Element.Keyed
import Html.Attributes
import Html.Events
import Json.Decode


column :
    List (Attribute msg)
    ->
        { data : List a
        , toKey : a -> String
        , toSize : a -> Int
        , scrollOffset : ScrollOffset
        , header :
            Maybe
                { height : Int
                , body : Element msg
                }
        , footer :
            Maybe
                { height : Int
                , body : Element msg
                }
        , view : Int -> a -> Element msg
        , noData : () -> Element msg
        , onScroll : ScrollOffset -> msg
        }
    -> Element msg
column attrs a =
    let
        list : VirtualList a
        list =
            compute
                a.data
                a.toSize
                (a.header |> Maybe.map .height |> Maybe.withDefault 0)
                a.scrollOffset

        header : Element msg
        header =
            case a.header of
                Just b ->
                    el
                        [ width fill
                        , height (px b.height)
                        , htmlAttribute (Html.Attributes.style "position" "sticky")
                        , htmlAttribute (Html.Attributes.style "z-index" "1")
                        , htmlAttribute (Html.Attributes.style "top" "0")
                        ]
                        b.body

                Nothing ->
                    none

        footer : Element msg
        footer =
            case a.footer of
                Just b ->
                    el [ width fill, height (px b.height) ] b.body

                Nothing ->
                    none
    in
    el
        (width fill :: height fill :: scrollbars :: onScroll a.scrollOffset a.onScroll :: attrs)
        (Element.Keyed.column
            [ width fill ]
            ([]
                ++ [ ( "zps6-header", header )
                   , ( "zps6-top", el [ height (px list.top) ] none )
                   ]
                ++ (if list.items == [] then
                        [ ( "zps6-data", a.noData () )
                        ]

                    else
                        list.items
                            |> List.map
                                (\x ->
                                    ( a.toKey x.value
                                    , el [ width fill, height (px x.size) ] (a.view x.index x.value)
                                    )
                                )
                   )
                ++ [ ( "zps6-bottom", el [ height (px list.bottom) ] none )
                   , ( "zps6-footer", footer )
                   ]
            )
        )



--


type ScrollOffset
    = ScrollOffset Int



--


type alias VirtualList a =
    { items : List (Item a)
    , top : Int
    , bottom : Int
    }


type alias Item a =
    { index : Int
    , size : Int
    , value : a
    }


viewportSize : number
viewportSize =
    1000


compute : List a -> (a -> Int) -> Int -> ScrollOffset -> VirtualList a
compute data toSize startOffset (ScrollOffset scrollOffset) =
    let
        offsetVisible : { min : Int, max : Int } -> Bool
        offsetVisible =
            intersects
                { min = scrollOffset - viewportSize
                , max = scrollOffset + viewportSize + viewportSize
                }

        fold : a -> ( VirtualList a, Int, Int ) -> ( VirtualList a, Int, Int )
        fold b ( acc, i, offset ) =
            let
                size : Int
                size =
                    max 0 (toSize b)
            in
            ( if offsetVisible { min = offset, max = offset + size } then
                { acc | items = { index = i, size = size, value = b } :: acc.items }

              else if acc.items == [] then
                { acc | top = acc.top + size }

              else
                { acc | bottom = acc.bottom + size }
            , i + 1
            , offset + size
            )
    in
    data
        |> List.foldl fold ( VirtualList [] 0 0, 0, startOffset )
        |> (\( x, _, _ ) -> { x | items = List.reverse x.items })


intersects : { min : number, max : number } -> { min : number, max : number } -> Bool
intersects a b =
    (b.min <= a.max) && (b.max >= a.min)


onScroll : ScrollOffset -> (ScrollOffset -> msg) -> Attribute msg
onScroll (ScrollOffset actual) toMsg =
    let
        decoder : Json.Decode.Decoder msg
        decoder =
            Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float
                |> Json.Decode.map (\x -> round (x / viewportSize) * viewportSize)
                |> Json.Decode.andThen
                    (\v ->
                        if v == actual then
                            Json.Decode.fail "Scroll offset not changed."

                        else
                            Json.Decode.succeed v
                    )
                |> Json.Decode.map (ScrollOffset >> toMsg)
    in
    htmlAttribute (Html.Events.on "scroll" decoder)
