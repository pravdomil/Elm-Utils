module Element.Virtualized exposing (column)

import Element as E
import Element.Keyed
import Html.Events
import Json.Decode as Decode


column :
    List (E.Attribute msg)
    ->
        { data : List a
        , toKey : a -> String
        , toSize : a -> Int
        , paddingTop : Int
        , paddingBottom : Int
        , scrollOffset : Int
        , view : a -> E.Element msg
        , onScroll : Int -> msg
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


step =
    1000


compute : { b | data : List a, toSize : a -> Int, paddingTop : Int, paddingBottom : Int, scrollOffset : Int } -> VirtualList a
compute a =
    let
        offsetVisible =
            intersects
                { min = a.scrollOffset - step
                , max = a.scrollOffset + step + step
                }

        fold :
            a
            ->
                { offset : Int
                , paddingTop : Int
                , paddingBottom : Int
                , usePaddingBottom : Bool
                , items : List (Item a)
                }
            ->
                { offset : Int
                , paddingTop : Int
                , paddingBottom : Int
                , usePaddingBottom : Bool
                , items : List (Item a)
                }
        fold b acc =
            let
                itemSize : Int
                itemSize =
                    a.toSize b
            in
            if offsetVisible { min = acc.offset, max = acc.offset + itemSize } then
                { acc
                    | offset = acc.offset + itemSize
                    , usePaddingBottom = True
                    , items = { size = itemSize, value = b } :: acc.items
                }

            else if acc.usePaddingBottom then
                { acc
                    | offset = acc.offset + itemSize
                    , paddingBottom = acc.paddingBottom + itemSize
                }

            else
                { acc
                    | offset = acc.offset + itemSize
                    , paddingTop = acc.paddingTop + itemSize
                }
    in
    a.data
        |> List.foldl fold
            { offset = a.paddingTop
            , paddingTop = a.paddingTop
            , paddingBottom = a.paddingBottom
            , usePaddingBottom = False
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


onScroll : Int -> (Int -> msg) -> E.Attribute msg
onScroll actual toMsg =
    let
        decoder : Decode.Decoder msg
        decoder =
            Decode.at [ "target", "scrollTop" ] Decode.float
                |> Decode.map (\v -> round (v / step) * step)
                |> Decode.andThen
                    (\v ->
                        if v == actual then
                            Decode.fail "Scroll offset not changed."

                        else
                            Decode.succeed v
                    )
                |> Decode.map toMsg
    in
    E.htmlAttribute (Html.Events.on "scroll" decoder)
