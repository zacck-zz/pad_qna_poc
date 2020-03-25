module Main exposing (main)

import Base64
import Browser
import Browser.Navigation as Nav

import Html
import Json.Encode exposing (Value)
import Page exposing (view)
import Page.AnswerDashboard as AnswerDashboard
import Page.Blank as Blank
import Page.Landing as Landing
import Page.NotFound as NotFound
import Route exposing (Route, urlToRoute)
import Session exposing (Session)
import Url


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        decoded_session =
            flags
                |> Session.decode key
    in
    changeRouteTo (Just (Route.urlToRoute url))
        (Redirect decoded_session)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Sub.none

        Landing landing ->
            Sub.map GotLandingMsg (Landing.subscriptions landing)

        AnswerDashboard answersModel ->
            Sub.map GotAnswerMsg (AnswerDashboard.subscriptions answersModel)


type Model
    = Redirect Session
    | NotFound Session
    | Landing Landing.Model
    | AnswerDashboard AnswerDashboard.Model




type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Ignored
    | GotLandingMsg Landing.Msg
    | GotAnswerMsg AnswerDashboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.load (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                maybeRoute =
                    url
                        |> Route.urlToRoute
                        |> Just
            in
            changeRouteTo maybeRoute model

        ( GotLandingMsg subMsg, Landing landing ) ->
            Landing.update subMsg landing
                |> updateWith Landing GotLandingMsg model

        ( GotAnswerMsg subMsg, AnswerDashboard answerDashboard ) ->
            AnswerDashboard.update subMsg answerDashboard
                |> updateWith AnswerDashboard GotAnswerMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Landing landingModel ->
            viewPage Page.Landing GotLandingMsg (Landing.view landingModel)

        AnswerDashboard answersModel ->
            viewPage Page.AnswerDashboard GotAnswerMsg (AnswerDashboard.view answersModel)


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Landing ->
            Landing.init session
                |> updateWith Landing GotLandingMsg model

        Just (Route.AnswerDashboard phone) ->
            AnswerDashboard.init phone session
                |> updateWith AnswerDashboard GotAnswerMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
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
            Landing.toSession session

        AnswerDashboard session ->
            AnswerDashboard.toSession session
