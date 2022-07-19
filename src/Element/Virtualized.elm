module Element.Virtualized exposing (ScrollOffset(..), column)

import Element as E
import Element.Keyed
import Html.Events
import Json.Decode


type ScrollOffset
    = ScrollOffset Int


column :
    List (E.Attribute msg)
    ->
        { data : List a
        , toKey : a -> String
        , toSize : a -> Int
        , paddingTop : Int
        , paddingBottom : Int
        , scrollOffset : ScrollOffset
        , view : a -> E.Element msg
        , onScroll : ScrollOffset -> msg
        }
    -> E.Element msg
column attrs a =
    let
        list : VirtualList a
        list =
            compute a
    in
    E.el
        (E.width E.fill
            :: E.height E.fill
            :: E.scrollbars
            :: onScroll a.scrollOffset a.onScroll
            :: attrs
        )
        (Element.Keyed.column
            (E.width E.fill
                :: E.height E.fill
                :: E.paddingEach list.padding
                :: E.spacing 0
                :: []
            )
            (list.items
                |> List.map
                    (\v ->
                        ( a.toKey v.value
                        , E.el [ E.width E.fill, E.height (E.px v.size) ] (a.view v.value)
                        )
                    )
            )
        )



--


type alias VirtualList a =
    { padding :
        { left : Int
        , right : Int
        , top : Int
        , bottom : Int
        }
    , items : List (Item a)
    }


type alias Item a =
    { size : Int
    , value : a
    }


viewportSize =
    1000


compute : { b | data : List a, toSize : a -> Int, paddingTop : Int, paddingBottom : Int, scrollOffset : ScrollOffset } -> VirtualList a
compute a =
    let
        scrollOffset =
            a.scrollOffset |> (\(ScrollOffset v) -> v)

        offsetVisible =
            intersects
                { min = scrollOffset - viewportSize
                , max = scrollOffset + viewportSize + viewportSize
                }

        fold :
            a
            -> { offset : Int, paddingTop : Int, paddingBottom : Int, items : List (Item a) }
            -> { offset : Int, paddingTop : Int, paddingBottom : Int, items : List (Item a) }
        fold b acc =
            let
                itemSize : Int
                itemSize =
                    a.toSize b
            in
            if offsetVisible { min = acc.offset, max = acc.offset + itemSize } then
                { acc
                    | offset = acc.offset + itemSize
                    , items = { size = itemSize, value = b } :: acc.items
                }

            else if List.head acc.items == Nothing then
                { acc
                    | offset = acc.offset + itemSize
                    , paddingTop = acc.paddingTop + itemSize
                }

            else
                { acc
                    | offset = acc.offset + itemSize
                    , paddingBottom = acc.paddingBottom + itemSize
                }
    in
    a.data
        |> List.foldl fold
            { offset = a.paddingTop
            , paddingTop = a.paddingTop
            , paddingBottom = a.paddingBottom
            , items = []
            }
        |> (\v ->
                { padding =
                    { left = 0
                    , right = 0
                    , top = v.paddingTop
                    , bottom = v.paddingBottom
                    }
                , items = List.reverse v.items
                }
           )


intersects : { min : number, max : number } -> { min : number, max : number } -> Bool
intersects a b =
    (b.min <= a.max) && (b.max >= a.min)


onScroll : ScrollOffset -> (ScrollOffset -> msg) -> E.Attribute msg
onScroll (ScrollOffset actual) toMsg =
    let
        decoder : Json.Decode.Decoder msg
        decoder =
            Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float
                |> Json.Decode.map (\v -> round (v / viewportSize) * viewportSize)
                |> Json.Decode.andThen
                    (\v ->
                        if v == actual then
                            Json.Decode.fail "Scroll offset not changed."

                        else
                            Json.Decode.succeed v
                    )
                |> Json.Decode.map (ScrollOffset >> toMsg)
    in
    E.htmlAttribute (Html.Events.on "scroll" decoder)
