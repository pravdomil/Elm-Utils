module Browser.QueryRouter exposing (..)

import Browser
import Browser.Navigation
import Url


type alias QueryRouter a =
    { key : Browser.Navigation.Key
    , baseUrl : Url.Url
    , state : a
    }


init : (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> QueryRouter a
init toState url key =
    { key = key
    , baseUrl = { url | query = Nothing, fragment = Nothing }
    , state = toState url
    }


urlRequested : Browser.UrlRequest -> { model | router : QueryRouter a } -> ( { model | router : QueryRouter a }, Cmd msg )
urlRequested req model =
    ( model
    , case req of
        Browser.Internal url ->
            if { url | query = Nothing, fragment = Nothing } == model.router.baseUrl then
                Browser.Navigation.pushUrl model.router.key (Url.toString url)

            else
                Browser.Navigation.load (Url.toString url)

        Browser.External url ->
            Browser.Navigation.load url
    )


urlChanged : (Url.Url -> a) -> Url.Url -> { model | router : QueryRouter a } -> ( { model | router : QueryRouter a }, Cmd msg )
urlChanged toState url model =
    ( { model
        | router = (\x -> { x | state = toState url }) model.router
      }
    , Cmd.none
    )
