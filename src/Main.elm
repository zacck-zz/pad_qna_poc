port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode
import Base64
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http exposing (bytesPart, multipartBody, stringPart)
import Task
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
  ({audio = Nothing, resp = "Nothign Uploaded"}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ consumeAudio UploadAnswer ]

type alias Model =
  { audio : Maybe Bytes
  , resp : String
  }

type Msg =
  StartRecording
  | StopRecording
  | UploadAnswer String
  | WavRequested
  | WavLoaded File
  | ByteUploadedFile Bytes
  | SendToServer
  | GotUploadResponse (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartRecording ->
      let
          start =
            startRecording ()
      in
      ({ model | resp = "Recording..."}, start)

    StopRecording ->
      let
          stop =
            stopRecording ()
      in
      ({ model | resp = "Recording Done ..."}, stop)

    UploadAnswer ans ->
      let
          byted =
            Base64.toBytes ans
      in
      ({ model | audio = byted, resp = "Recording Ready for Upload"} , Cmd.none)

    WavRequested ->
      (model
      , Select.file ["audio/wav"] WavLoaded
      )

    WavLoaded file ->
      (model
      , Task.perform ByteUploadedFile (File.toBytes file)
      )

    ByteUploadedFile bytes ->
      ({ model | audio = Just bytes, resp = "File Ready for Upload"}, Cmd.none)

    SendToServer ->
      case model.audio of
        Nothing ->
          (model, Cmd.none) {- Should be covered using error handling -}

        Just b ->
          let
              body =
                multipartBody
                  [ bytesPart "audio" "audio/*" b
                  , stringPart "description" "some parts"
                  , stringPart "tags" " some tags"
                  ]
          in
              (model
              ,Http.post
                { url = "http://localhost:5000/add"
                , body = body
                , expect = Http.expectWhatever GotUploadResponse
                }
              )

    GotUploadResponse res ->
      case res of
        Ok _ ->
          ({model | resp = "uploaded"}, Cmd.none)

        Err _ ->
          ({model | resp = "We broke something"}, Cmd.none)

view : Model -> Html Msg
view model =
        div []
        [ h1 [] [ text ("Status: " ++ model.resp) ]
        , h1 [] [ text "Answer Recorder" ]
        , button [ onClick StartRecording ] [ text "Record"]
        , h1 [] [ text "            " ]
        , button [ onClick StopRecording] [ text "Stop" ]
        , h1 []  [ text "Upload file instead"]
        , button [ onClick WavRequested] [ text "Upload Audio" ]
        , h1 []  [ text "        "]
        , button [ onClick SendToServer ] [text "Upload Answer"]
        ]



port startRecording : () -> Cmd msg

port stopRecording : () -> Cmd msg

port consumeAudio : (String -> msg) -> Sub msg
