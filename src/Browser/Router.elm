module Browser.Router exposing (..)

import Browser
import Browser.Navigation
import Url


type alias Router a =
    { key : Browser.Navigation.Key
    , baseUrl : Url.Url
    , state : a
    }


init : (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> Router a
init toState url key =
    Router
        key
        url
        (toState url)


urlRequested : Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlRequested req model =
    ( model
    , case req of
        Browser.Internal url ->
            if compareUrl url model.router.baseUrl then
                Browser.Navigation.pushUrl model.router.key (Url.toString url)

            else
                Browser.Navigation.load (Url.toString url)

        Browser.External url ->
            Browser.Navigation.load url
    )


urlChanged : (Url.Url -> a) -> Url.Url -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlChanged toState url model =
    ( { model
        | router = (\x -> { x | state = toState url }) model.router
      }
    , Cmd.none
    )



--


compareUrl : Url.Url -> Url.Url -> Bool
compareUrl a b =
    { a | query = Nothing, fragment = Nothing }
        == { b | query = Nothing, fragment = Nothing }
