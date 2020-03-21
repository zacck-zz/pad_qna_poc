module Route exposing (Route(..), replaceUrl, urlToRoute)

import Browser.Navigation as Nav
import Url
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Landing
    | AnswerDashboard (Maybe String)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toPath route)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map AnswerDashboard (s "answer-dashboard" <?> Query.string "owner")
        ]


toPath : Route -> String
toPath route =
    let
        path =
            case route of
                Landing ->
                    []
                        |> UrlBuilder.absolute []

                AnswerDashboard maybePhone ->
                    let
                        phone =
                            maybePhone
                                |> Maybe.withDefault "master"

                        p =
                            [ UrlBuilder.string "owner" phone ]
                                |> UrlBuilder.absolute [ "answer-dashboard" ]
                    in
                    p
    in
    path


urlToRoute : Url.Url -> Route
urlToRoute url =
    url
        |> Parser.parse parser
        |> Maybe.withDefault Landing
