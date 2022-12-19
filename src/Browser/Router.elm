module Browser.Router exposing (Router, init, requestUrl, sameDirectory, samePath, update)

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


requestUrl : (Url.Url -> Url.Url -> Bool) -> Browser.UrlRequest -> { model | router : Router a } -> ( { model | router : Router a }, Cmd msg )
requestUrl isInternalUrl req model =
    ( model
    , case req of
        Browser.Internal url ->
            if isInternalUrl model.router.baseUrl url then
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


samePath : Url.Url -> Url.Url -> Bool
samePath a b =
    (a.protocol == b.protocol)
        && (a.host == b.host)
        && (a.port_ == b.port_)
        && (a.path == b.path)


sameDirectory : Url.Url -> Url.Url -> Bool
sameDirectory a b =
    (a.protocol == b.protocol)
        && (a.host == b.host)
        && (a.port_ == b.port_)
        && (dropAfter "/" a.path == dropAfter "/" b.path)



--


dropAfter : String -> String -> String
dropAfter pattern a =
    String.indexes pattern a
        |> List.reverse
        |> List.head
        |> Maybe.map (\x -> String.left (x + 1) a)
        |> Maybe.withDefault ""
