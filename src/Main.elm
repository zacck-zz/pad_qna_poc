port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode
import Base64
import Json.Decode as JD exposing(Decoder, at, int, map3, map4,string)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, audio, button, div, form, h1, h2, h3, input, source, table, tbody, textarea, thead, td, th, text, tr, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, controls, cols, for, id, type_, src, rows, value)
import Http exposing (bytesPart, multipartBody, stringPart)
import Task
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
init _ url key =
  let
      initForm =
        { audio = Nothing
        , desc = ""
        , tags = ""
        }

      mod =
         { form = initForm
         , resp = "Nothign Uploaded"
         , answers = []
         , questions = []
         , key = key
         , url = url
         }

      cmds =
        Cmd.batch
          [ getAnswers
          , getQuestions
          ]

  in
  (mod, cmds)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ consumeAudio UploadAnswer ]

type alias Model =
  { form : Form
  , resp : String
  , answers : List Answer
  , questions : List Question
  , key : Nav.Key
  , url : Url.Url
  }

type alias Form =
  { audio : Maybe Bytes
  , desc : String
  , tags : String
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
  | GotAnswers (Result Http.Error (List Answer))
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotQuestions (Result Http.Error (List Question ))
  | SetTags String
  | SetDescription String
  | SendAnswer
  | AnswerSent (Result Http.Error ())

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

          form =
           model.form

          updatedForm =
            { form | audio = byted }
      in
      ({ model | form = updatedForm, resp = "Recording Ready for Upload"} , Cmd.none)

    WavRequested ->
      (model
      , Select.file ["audio/wav"] WavLoaded
      )

    WavLoaded file ->
      (model
      , Task.perform ByteUploadedFile (File.toBytes file)
      )

    ByteUploadedFile bytes ->
      let
          form =
            model.form

          updatedForm =
            { form | audio = Just bytes }
      in
      ({ model | form = updatedForm, resp = "File Ready for Upload"}, Cmd.none)

    SendToServer ->
      case model.form.audio of
        Nothing ->
          (model, Cmd.none) {- Should be covered using error handling -}

        Just b ->
          let
              body =
                multipartBody
                  [ bytesPart "audio" "audio/*" b
                  , stringPart "description" model.form.desc
                  , stringPart "tags" model.form.tags
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
          ({model | resp = "uploaded"}
          , getAnswers
          )

        Err _ ->
          ({model | resp = "We broke something"}, Cmd.none)

    GotAnswers res ->
      case res of
        Ok a ->
          ({ model | answers = a}, Cmd.none)

        Err e ->
          let
             _ = Debug.log "errors" e
          in
          ( {model | resp = "Problem when fetching answers"}
          , Cmd.none
          )

    GotQuestions res ->
      case res of
        Ok q ->
          ({ model | questions = q}
          , Cmd.none
          )

        Err err ->
          let
             _ = Debug.log "q err"  err
          in
          ({ model | resp = "Problem when fetching questions" }
          , Cmd.none
          )

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    SetTags tags ->
      let
         form =
          model.form

         updatedForm =
           { form | tags = tags }

      in
          ({model | form = updatedForm}
          , Cmd.none
          )

    SetDescription desc ->
      let
          form =
            model.form

          updatedForm =
            { form | desc = desc }
      in
          ({model | form = updatedForm}
          , Cmd.none
          )

    SendAnswer ->
       let
           reqBody =
             multipartBody
              [ stringPart "queue_owner" "master"
              , stringPart "answer_audio" "answer"
              , stringPart "question_ids" "ids"
              ]
       in
       (model
       ,Http.post
          { url = "http://localhost:5000/answer"
          , body = reqBody
          , expect = Http.expectWhatever AnswerSent
          }
       )

    AnswerSent _ ->
      (model, Cmd.none)

type alias Question =
  { q_audio: String
  , q_id: Int
  , q_meta: String
  }

questionDecoder : Decoder Question
questionDecoder =
  map3 Question
    (at ["q_audio"] string)
    (at ["q_id"] int)
    (at ["q_meta"] string)

questionsDecoder : Decoder (List Question)
questionsDecoder =
  JD.list questionDecoder

getQuestions : Cmd Msg
getQuestions =
  Http.get
    { url = "http://localhost:5000/list-questions/master"
    , expect = Http.expectJson GotQuestions questionsDecoder
    }

type alias Answer =
  { audio_url: String
  , description: String
  , fname: String
  , tags: String
  }

answerDecoder : Decoder Answer
answerDecoder =
  map4 Answer
    (at ["audio_url"] string)
    (at ["description"] string)
    (at ["fname"] string)
    (at ["tags"] string)

getAnswers : Cmd Msg
getAnswers =
  Http.get
    { url = "http://localhost:5000/list"
    , expect = Http.expectJson GotAnswers answersDecoder
    }


answersDecoder : Decoder (List Answer)
answersDecoder =
  JD.list answerDecoder



view : Model -> Browser.Document Msg
view model =
  let
      views =
        div [ class "container" ]
        [ viewQuestions model
        , div [ id "send-col"]
              [ button [ id "send", onClick SendAnswer]
                       [ text "Send!" ]
              ]
        , viewAnswersSection model
        ]
  in
      { title = "tit", body = [views] }

viewAnswer : Answer -> Html msg
viewAnswer ans =
  tr []
     [ td [ class "td-check"] [ input [ type_ "radio", value "cotton"] [] ]
     , td [] [ text ans.description]
     , td [] [ text ans.tags]
     , td []
          [ audio [ controls True ]
                  [ source [ src ("http://localhost:5000/" ++ ans.audio_url) ] [] ]
          ]
     ]

viewAnswerList : Model -> Html msg
viewAnswerList model =
  div [ ]
      [ h2 [] [ text "Answers"]
      , div []
            [ label [ ] [ text "Search" ]
            , input [ type_ "text", value ""] []
            , input [ type_ "text", value ""] []
            ]
      , table []
              [ thead []
                      [ tr []
                           [ th [ class "td-check" ] [ text "Send" ] ]
                           , th [] [ text "Description"]
                           , th [] [ text "Answer" ]
                      ]
              , tbody []
                      (List.map viewAnswer model.answers)
              ]
      ]


viewAnswersSection : Model -> Html Msg
viewAnswersSection model =
  div [ id "answers"]
    [ h1 [] [ text ("Status: " ++ model.resp) ]
    , div [ id "add-answer" ]
           [ h3 [] [ text "Add Answer:"]
           , div []
                 [ label [ for "description" ] [ text "Description" ]
                 , textarea [ cols 80, rows 4, onInput SetDescription ]  []
                 ]
           , div []
                 [ label [ for "tags" ] [ text "Tags" ]
                 , input [ type_ "text", onInput SetTags ] []
                 ]
           , div []
                 [ h1 [] [ text "Answer Recorder" ]
                 , button [ onClick StartRecording ] [ text "Record"]
                 , button [ onClick StopRecording] [ text "Stop" ]
                 ]
           , div []
                 [ h1 []  [ text "Upload file instead"]
                 , button [ onClick WavRequested] [ text "Upload Audio" ]
                 ]

           , div []
                 [ button [ onClick SendToServer ] [text "Upload Answer"] ]
           ]
    , viewAnswerList model
    ]

viewQuestion : Question -> Html msg
viewQuestion ques =
         tr []
            [ td [ class "td-check"] [ input [type_ "checkbox", value "rice" ] [] ]
            , td [ ] [ text ques.q_meta]
            , td [ ] [ text (ques.q_id |> String.fromInt) ]
            , td [ ] [ audio [ controls True]
                         [ source [src ("http://localhost:5000/" ++ ques.q_audio), type_ "audio/wav" ] [] ]
                     ]
            ]


viewQuestions : Model -> Html msg
viewQuestions model =
  div [ id "questions" ]
      [ h2 [] [ text "Questions" ]
      , table []
        [ thead []
                [ tr []
                     [ th [ class "td-check" ]
                          [ text "Send" ]
                     , th [ ]
                          [ text "Phone" ]
                     , th []
                          [ text "Received" ]
                     ]
                ]
        , tbody []
                (List.map viewQuestion model.questions)
        ]
     ]


port startRecording : () -> Cmd msg

port stopRecording : () -> Cmd msg

port consumeAudio : (String -> msg) -> Sub msg
