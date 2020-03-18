module Route exposing (Route(..), replaceUrl, urlToRoute)

import Browser.Navigation as Nav
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query


type Route =
  Landing
  | AnswerDashboard (Maybe String)

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
  Nav.replaceUrl key (toPath route)


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Landing Parser.top
    , Parser.map AnswerDashboard ( s "answer-dashboard" <?> Query.string "queue")
    ]

toPath : Route -> String
toPath route =
  let
      path =
        case route of
          Landing ->
            [""]

          AnswerDashboard phone ->
            let
                params =
                  phone
                  |> Maybe.withDefault ""
            in
            ["answer-dashboard", params ]

  in
      String.join "/" path

urlToRoute : Url.Url -> Route
urlToRoute url =
  url
  |> Parser.parse parser
  |> Maybe.withDefault Landing
