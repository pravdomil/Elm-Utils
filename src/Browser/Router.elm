module Browser.Router exposing (Router, baseUrl, init, key, requestUrl, state, updateState, urlToDirectoryBaseUrl, urlToFileBaseUrl)

import Browser
import Browser.Navigation
import Url


type Router a
    = Router Browser.Navigation.Key Url.Url a


key : Router a -> Browser.Navigation.Key
key (Router a _ _) =
    a


baseUrl : Router a -> Url.Url
baseUrl (Router _ a _) =
    a


state : Router a -> a
state (Router _ _ a) =
    a



--


init : (Url.Url -> Url.Url) -> (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> Router a
init toBaseUrl toState url key_ =
    Router
        key_
        (toBaseUrl url)
        (toState url)


requestUrl : (Url.Url -> Url.Url) -> Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
requestUrl toBaseUrl req model =
    ( model
    , case req of
        Browser.Internal url ->
            if toBaseUrl url == baseUrl model.router then
                Browser.Navigation.pushUrl (key model.router) (Url.toString url)

            else
                Browser.Navigation.load (Url.toString url)

        Browser.External url ->
            Browser.Navigation.load url
    )


updateState : (Url.Url -> a) -> Url.Url -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
updateState toState url model =
    ( { model
        | router = (\(Router x x2 _) -> Router x x2 (toState url)) model.router
      }
    , Cmd.none
    )



--


urlToFileBaseUrl : Url.Url -> Url.Url
urlToFileBaseUrl a =
    { a
        | query = Nothing
        , fragment = Nothing
    }


urlToDirectoryBaseUrl : Url.Url -> Url.Url
urlToDirectoryBaseUrl a =
    { a
        | path = dropAfter "/" a.path
        , query = Nothing
        , fragment = Nothing
    }



--


dropAfter : String -> String -> String
dropAfter pattern a =
    String.indexes pattern a
        |> List.reverse
        |> List.head
        |> Maybe.map (\x -> String.left (x + 1) a)
        |> Maybe.withDefault ""
