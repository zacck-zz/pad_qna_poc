module Page.Landing exposing (Model, Msg, init, toSession, update, view)


import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Profile exposing (phoneToString)
import Session exposing (Session)
import Route exposing (Route)


type alias Model =
  { queue : String
  , session : Session
  }

type Msg =
  SetQueue String

init : Session -> (Model, Cmd Msg)
init sess =
  let
      model =
        { session = sess
        , queue = ""
        }

      cmd =
        case Session.cred sess of
          Just cred ->
            let
               maybePhone =
                sess
                |> Session.phoneString
                |> Just
            in
            Route.replaceUrl (Session.navKey sess) (Route.AnswerDashboard maybePhone)

          Nothing ->
            Cmd.none
  in
      (model, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetQueue q ->
      ({ model | queue = q }, Cmd.none)


view : Model -> { title : String, body : Html msg }
view _ =
  let
      b =
        div [ class "container"]
            [ text "Landing Page" ]
  in
      { title  = "Welcome"
      , body = b
      }


toSession : Model -> Session
toSession { session } =
  session

