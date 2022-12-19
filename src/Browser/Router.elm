module Browser.Router exposing (Router, init, requestUrl, update, urlToDirectoryBaseUrl, urlToFileBaseUrl)

import Browser
import Browser.Navigation
import Url


type alias Router a =
    { key : Browser.Navigation.Key
    , baseUrl : Url.Url
    , state : a
    }


init : (Url.Url -> Url.Url) -> (Url.Url -> a) -> Url.Url -> Browser.Navigation.Key -> Router a
init toBaseUrl toState url key =
    Router
        key
        (toBaseUrl url)
        (toState url)


requestUrl : (Url.Url -> Url.Url) -> Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
requestUrl toBaseUrl req model =
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


update : (Url.Url -> a) -> Url.Url -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
update toState url model =
    ( { model
        | router = (\x -> { x | state = toState url }) model.router
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
