port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import File exposing (File)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Url exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init _  =
  ({answer = Nothing }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ consumeAudio UploadAnswer ]

type alias Model =
  { answer : Maybe String }

type Msg =
  StartRecording
  | StopRecording
  | UploadAnswer String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartRecording ->
      let
          start =
            startRecording ()
      in
      (model, start)

    StopRecording ->
      let
          stop =
            stopRecording ()
      in
      (model, stop)

    UploadAnswer ans ->
      let
          newModel = { model | answer = Just ans}
      in
      (newModel, Cmd.none)
view : Model -> Html Msg
view model =
        div []
        [ h1 [] [ text "Answer Recorder" ]
        , button [ onClick StartRecording ] [ text "Record"]
        , h1 [] [ text "            " ]
        , button [ onClick StopRecording] [ text "Stop" ]
        ]


port startRecording : () -> Cmd msg

port stopRecording : () -> Cmd msg

port consumeAudio : (String -> msg) -> Sub msg
