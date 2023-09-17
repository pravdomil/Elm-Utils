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
init toBaseUrl toState url key =
    Router
        key
        (toBaseUrl url)
        (toState url)


urlRequested : (Url.Url -> Url.Url) -> Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlRequested toBaseUrl a model =
    ( model
    , case a of
        Browser.Internal b ->
            if toBaseUrl b == model.router.baseUrl then
                Browser.Navigation.pushUrl model.router.key (Url.toString b)

            else
                Browser.Navigation.load (Url.toString b)

        Browser.External b ->
            Browser.Navigation.load b
    )


urlChanged : (Url.Url -> a) -> Url.Url -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
urlChanged toState a model =
    ( updateState (toState a) model
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
dropAfter needle a =
    case List.head (List.reverse (String.indexes needle a)) of
        Just b ->
            String.left (b + 1) a

        Nothing ->
            a
