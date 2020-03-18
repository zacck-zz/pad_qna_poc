module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, audio, button, div, form, h1, h2, h3, input, p, source, table, tbody, textarea, thead, td, th, text, tr, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (checked, class, controls, cols, for, id, name, type_, src, rows, value)
import Page exposing (Page, view)
import Page.Landing as Landing
import Page.AnswerDashboard as AnswerDashboard
import Route exposing (Route, urlToRoute)
import Session exposing (Session)
import Url exposing (..)


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
      decoded_session =
        flags
        |> Session.decode key
  in
      changeRouteTo ( Route.urlToRoute url)
        (Redirect decoded_session)



subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

type Model =
  Redirect Session
  | NotFound Session
  | Landing Landing.Model
  | AnswerDashboard AnswerDashboard.Model




type alias RoutingData =
  { key : Nav.Key
  , url : Url.Url
  , current : Route
  }


type Msg =
  LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | Ignored
  | GotLandingMsg Landing.Msg
  | GotAnswerMsg AnswerDashboard.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (Ignored, _) ->
      (model, Cmd.none)

    (LinkClicked urlRequest, _) ->
        case urlRequest of
          Browser.Internal url ->
            ( model, Nav.load (Url.toString url) )

          Browser.External href ->
            ( model, Nav.load href )

    (UrlChanged url, _) ->
      changeRouteTo (Route.urlToRoute url) model




view : Model -> Browser.Document Msg
view model =
  let
     viewPage page toMsg config =
      let
          {title, body} =
           Page.view page config
      in
          { title = title
          , body = List.map (Html.map toMsg) body
          }
  in
      case model of
        Landing landingModel ->
         viewPage Page.Landing GotLandingMsg (Landing.view landingModel)

        AnswerDashboard answersModel ->
         viewPage Page.AnswerDashboard GotAnswerMsg (AnswerDashboard.view answersModel)

changeRouteTo : Maybe Route -> Model -> (Model, Cmd Msg)
changeRouteTo maybeRoute model =
  let
      session =
        toSession model
  in
      case maybeRoute of
        Nothing ->
          (NotFound session, Cmd.none)

        Just Route.Landing ->
          Landing.init session
            |> updateWith Landing GotLandingMsg model

        Just (Route.AnswerDashboard phone) ->
          AnswerDashboard.init phone session
            |> updateWith AnswerDashboard GotAnswerMsg model

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> (subModel, Cmd subMsg) -> ( Model, Cmd Msg)
updateWith toModel toMsg model (subModel, subCmd) =
  ( toModel subModel
  , Cmd.map toMsg subCmd
  )


toSession : Model -> Session
toSession page =
  case page of
    NotFound session ->
      session

    Redirect session ->
      session

    Landing session ->
      session

    AnswerDashboard session ->
      session
