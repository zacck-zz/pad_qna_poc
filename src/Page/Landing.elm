module Page.Landing exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (Session)


type alias Model =
    { queue : String
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init sess =
    let
        model =
            { session = sess
            , queue = ""
            }

        cmd =
            case Session.cred sess of
                Just _ ->
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
    ( model, cmd )


type Msg
    = SetQueue String
    | GoToQueue
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQueue q ->
            ( { model | queue = q }
            , Cmd.none
            )

        GoToQueue ->
            if String.isEmpty model.queue then
                ( model, Cmd.none )

            else
                let
                    profileData =
                        { phone = model.queue }

                    loginData =
                        { token = model.queue
                        , user = profileData
                        }
                in
                ( model
                , Session.login loginData
                )

        GotSession sess ->
            let
                updatedModel =
                    { model | session = sess }

                routeCmd =
                    model.queue
                        |> Just
                        |> Route.AnswerDashboard
                        |> Route.replaceUrl (Session.navKey sess)
            in
            ( updatedModel, routeCmd )


view : Model -> { title : String, body : Html Msg }
view _ =
    let
        b =
            div [ class "container" ]
                [ div [ class "landing-page" ]
                    [ div [ class "file-form" ]
                        [ label [] [ text "Phone Number:" ]
                        , input [ type_ "text", onInput SetQueue ] []
                        ]
                    , div [ class "file-form" ]
                        [ button [] [ text "Master" ]
                        , button [ onClick GoToQueue ] [ text "Set Queue" ]
                        ]
                    ]
                ]
    in
    { title = "Welcome"
    , body = b
    }


toSession : Model -> Session
toSession { session } =
    session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
