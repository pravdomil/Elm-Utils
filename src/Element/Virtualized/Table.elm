module Element.Virtualized.Table exposing (..)

import Element exposing (..)
import Element.Virtualized


table :
    List (Attribute msg)
    ->
        { data : List a
        , toKey : a -> String
        , toSize : a -> Int
        , toContext : Int -> a -> context

        --
        , columns : List (Column context a msg)
        , emptyData : () -> Element msg
        , viewRow : context -> a -> List (Attribute msg) -> List (Element msg) -> Element msg

        --
        , header :
            { height : Int
            , attributes : List (Attribute msg)
            }
        , footer :
            Maybe
                { height : Int
                , body : Element msg
                }

        --
        , scrollOffset : Element.Virtualized.ScrollOffset
        , onScroll : Element.Virtualized.ScrollOffset -> msg
        }
    -> Element msg
table attrs a =
    let
        totalWidth : Int
        totalWidth =
            a.columns |> List.foldl (\x acc -> acc + x.width) 0

        header : Element msg
        header =
            row (width (fill |> minimum totalWidth) :: height fill :: a.header.attributes)
                (a.columns |> List.map (\x -> el [ width (px x.width), height fill ] x.header))

        view : Int -> a -> Element msg
        view i b =
            let
                context : context
                context =
                    a.toContext i b
            in
            a.viewRow
                context
                b
                [ width (fill |> minimum totalWidth), height fill ]
                (a.columns |> List.map (\x -> el [ width (px x.width), height fill ] (x.view context b)))
    in
    Element.Virtualized.column
        attrs
        { data = a.data
        , toKey = a.toKey
        , toSize = a.toSize
        , scrollOffset = a.scrollOffset
        , header =
            Just
                { height = a.header.height
                , body = header
                }
        , footer = a.footer
        , view = view
        , emptyData = a.emptyData
        , onScroll = a.onScroll
        }



--


type alias Column context a msg =
    { header : Element msg
    , width : Int
    , view : context -> a -> Element msg
    }
