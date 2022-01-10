module Browser.QueryRouter exposing (..)

import Browser
import Browser.Navigation
import Platform.Extra
import Url


type alias QueryRouter a =
    { key : Browser.Navigation.Key
    , baseUrl : Url.Url
    , state : a
    }



--


init : (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> (Url.Url -> msg) -> ( QueryRouter a, Cmd msg )
init toState url key urlChanged_ =
    ( { key = key
      , baseUrl = { url | query = Nothing, fragment = Nothing }
      , state = toState url
      }
    , Platform.Extra.sendMsg (urlChanged_ url)
    )


urlRequested : Browser.UrlRequest -> QueryRouter a -> Cmd msg
urlRequested req a =
    case req of
        Browser.Internal url ->
            if { url | query = Nothing, fragment = Nothing } == a.baseUrl then
                Browser.Navigation.pushUrl a.key (Url.toString url)

            else
                Browser.Navigation.load (Url.toString url)

        Browser.External url ->
            Browser.Navigation.load url


urlChanged : (Url.Url -> a) -> Url.Url -> QueryRouter a -> QueryRouter a
urlChanged toState url a =
    { a
        | state = toState url
    }
