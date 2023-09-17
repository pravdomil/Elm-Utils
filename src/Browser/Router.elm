module Browser.Router exposing (Router, directoryBaseUrl, fileBaseUrl, init, urlChanged, urlRequested)

import Browser
import Browser.Navigation
import Url


type alias Router a =
    { key : Browser.Navigation.Key
    , baseUrl : Url.Url
    , state : a
    }



--


init : (Url.Url -> Url.Url) -> (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> Router a
init toBaseUrl toState url key_ =
    Router
        key_
        (toBaseUrl url)
        (toState url)


urlRequested : (Url.Url -> Url.Url) -> Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlRequested toBaseUrl req model =
    ( model
    , case req of
        Browser.Internal url ->
            if toBaseUrl url == model.router.baseUrl then
                Browser.Navigation.pushUrl model.router.key (Url.toString url)

            else
                Browser.Navigation.load (Url.toString url)

        Browser.External url ->
            Browser.Navigation.load url
    )


urlChanged : (Url.Url -> a) -> Url.Url -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlChanged toState url model =
    ( updateState (toState url) model
    , Cmd.none
    )



--


fileBaseUrl : Url.Url -> Url.Url
fileBaseUrl a =
    { a
        | query = Nothing
        , fragment = Nothing
    }


directoryBaseUrl : Url.Url -> Url.Url
directoryBaseUrl a =
    { a
        | path = dropAfter "/" a.path
        , query = Nothing
        , fragment = Nothing
    }



--


updateState : a -> { model | router : Router a } -> { model | router : Router a }
updateState a model =
    { model
        | router = (\x -> { x | state = a }) model.router
    }



--


dropAfter : String -> String -> String
dropAfter pattern a =
    case List.head (List.reverse (String.indexes pattern a)) of
        Just b ->
            String.left (b + 1) a

        Nothing ->
            ""
