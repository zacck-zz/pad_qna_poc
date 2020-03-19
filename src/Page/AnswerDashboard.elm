port module Page.AnswerDashboard exposing (Model, Msg, init, toSession, update, view)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as BDecode
import Html exposing (Html, audio, button, div, form, h1, h2, h3, input, p, source, table, tbody, textarea, thead, td, th, text, tr, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (checked, class, controls, cols, for, id, name, type_, src, rows, value)
import Json.Decode as JD exposing(Decoder, at, int, map3, map4,string)
import Json.Encode as Encode
import File exposing (File)
import File.Select as Select
import Http exposing (bytesPart, filePart, jsonBody, multipartBody, stringPart)
import Session exposing (Session)
import Url.Builder as UrlBuilder
import Task


type alias Model =
  { answerForm : AnswerForm
  , sendForm : SendForm
  , searchForm : SearchForm
  , answers : List Answer
  , questions : List Question
  , session : Session
  }

type  AnswerFormStatus =
  NoData
  | Recording
  | Recorded
  | Selecting
  | Selected

type alias AnswerForm =
  { status : AnswerFormStatus
  , audio : Maybe AudioResource
  , desc : String
  , tags : String
  , audio_url : Maybe String
  }

type alias SendForm =
  { queue_owner : String
  , answer_audio : String
  , question_ids : List Int
  }

type alias SearchForm =
  { tags : String
  , description : String
  }

type AudioResource =
  F File
  |R Bytes

init : Session -> (Model, Cmd Msg)
init session =
  let
      mod =
        initModel session
  in
      (mod, Cmd.none)


initModel : Session -> Model
initModel sess =
  let
      mod =
        { session = sess
        , answerForm = initAnswerForm
        , sendForm = initSendForm
        , searchForm = initSearchForm
        , answers  = []
        , questions = []
        }
  in
      mod

initAnswerForm : AnswerForm
initAnswerForm =
  { status = NoData
  , audio = Nothing
  , desc = ""
  , tags = ""
  , audio_url = Nothing
  }

initSendForm : SendForm
initSendForm =
        { answer_audio = ""
        , queue_owner = "master"
        , question_ids = []
        }

initSearchForm : SearchForm
initSearchForm =
        { tags = ""
        , description = ""
        }



type Msg =
  StartRecording
  | StopRecording
  | UploadAnswer String
  | WavRequested
  | WavLoaded File
  | SendToServer
  | GotUploadResponse (Result Http.Error ())
  | GotAnswers (Result Http.Error (List Answer))
  | GotQuestions (Result Http.Error (List Question ))
  | SetTags String
  | SetDescription String
  | SendAnswer
  | AnswerSelected String
  | QuestionChecked String
  | AnswerSent (Result Http.Error (List Question))
  | SetAudioUrl String
  | ClearAudio
  | FilterAnswers
  | SetSearchDesc String
  | SetSearchTags String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartRecording ->
      let
          start =
            startRecording ()

          answerForm =
            model.answerForm

          updatedAnswerForm =
            { answerForm | status = Recording }
      in
      ({ model | answerForm = updatedAnswerForm }, start)

    StopRecording ->
      let
          stop =
            stopRecording ()

          answerForm =
            model.answerForm

          updatedAnswerForm =
            { answerForm | status = Recorded }
      in
      ({ model | answerForm = updatedAnswerForm}, stop)

    UploadAnswer ans ->
      let
          byted =
            Base64.toBytes ans

          audio_url =
            ("data:audio/ogg; codecs=opus;base64," ++ ans)
            |> Just

          answerForm =
           model.answerForm

          updatedForm =
            case byted of
              Just byts ->
                { answerForm | audio = Just (R byts) , audio_url = audio_url}

              Nothing ->
                answerForm
      in
      ({ model | answerForm = updatedForm} , Cmd.none)

    WavRequested ->
      let
          answerForm =
            model.answerForm

          updatedAnswerForm =
            { answerForm | status = Selecting }
      in
      ( { model | answerForm = updatedAnswerForm }
      , Select.file ["audio/wav"] WavLoaded
      )

    WavLoaded file ->
      let
          answerForm =
            model.answerForm

          updatedForm =
            { answerForm | audio = Just (F file), status = Selected }
      in
      ( { model | answerForm = updatedForm}
      , Task.perform SetAudioUrl (File.toUrl file)
      )


    SendToServer ->
      case model.answerForm.audio of
        Nothing ->
          (model, Cmd.none) {- Should be covered using error handling -}

        Just ar ->
          let
              files =
                case ar of
                  F f ->
                    filePart "audio"  f

                  R r ->
                    bytesPart "audio"  "audio/*" r

              body =
                multipartBody
                  [ files
                  , stringPart "description" model.answerForm.desc
                  , stringPart "tags" model.answerForm.tags
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
          ( { model | answerForm = initAnswerForm}
          , getAnswers
          )

        Err _ ->
          (model, Cmd.none)

    GotAnswers res ->
      case res of
        Ok a ->
          ({ model | answers = a}, Cmd.none)

        Err e ->
          ( model
          , Cmd.none
          )

    GotQuestions res ->
      case res of
        Ok q ->
          ({ model | questions = q}
          , Cmd.none
          )

        Err err ->
          ( model
          , Cmd.none
          )

    SetTags tags ->
      let
         answerForm =
          model.answerForm

         updatedForm =
           { answerForm | tags = tags }

      in
          ({model | answerForm = updatedForm}
          , Cmd.none
          )

    SetDescription desc ->
      let
          answerForm =
            model.answerForm

          updatedForm =
            { answerForm | desc = desc }
      in
          ({model | answerForm = updatedForm}
          , Cmd.none
          )

    SendAnswer ->
       let
           encodedList =
             model.sendForm.question_ids
             |> Encode.list Encode.int

           reqBody =
             Encode.object
                [ ("queue_owner", Encode.string model.sendForm.queue_owner)
                , ("answer_audio", Encode.string model.sendForm.answer_audio)
                , ("question_ids", encodedList)
                ]
             |> jsonBody

           sendAnswer : Cmd Msg
           sendAnswer =
             Http.post
              { url = "http://localhost:5000/answer"
              , body = reqBody
              , expect = Http.expectJson AnswerSent questionsDecoder
              }

       in
       (model, sendAnswer)

    AnswerSelected ans ->
      let
          sendForm =
            model.sendForm

          updatedSendForm =
            { sendForm | answer_audio = ans }
      in
          ({ model | sendForm = updatedSendForm }, Cmd.none)

    QuestionChecked stringId ->
      let
          sendForm =
            model.sendForm

          q_ids =
            sendForm.question_ids

          maybeId =
            stringId
            |>String.toInt

          updatedSendForm =
            case maybeId of
              Just id ->
                if List.member id q_ids then
                  { sendForm | question_ids = q_ids |> List.filter (\x -> x /= id) }
                else
                  { sendForm | question_ids = (id :: q_ids) }

              Nothing ->
                sendForm
      in
          ({ model | sendForm = updatedSendForm} , Cmd.none)

    AnswerSent res ->
      case res of
        Ok qs ->
          ( { model | questions = qs, sendForm = initSendForm }
          , Cmd.none
          )

        Err er ->
           ( model, Cmd.none)

    SetAudioUrl url ->
      let
          answerForm =
            model.answerForm

          updatedAnswerForm =
            { answerForm | audio_url = Just url }
      in
          ( { model | answerForm = updatedAnswerForm}
          , Cmd.none
          )

    ClearAudio ->
      let
          answerForm =
            model.answerForm

          updatedAnswerForm =
            { answerForm | status = NoData, audio = Nothing, audio_url = Nothing }

      in
          ( { model | answerForm = updatedAnswerForm }
          , Cmd.none
          )


    FilterAnswers ->
      let
         tags =
           model.searchForm.tags

         description =
           model.searchForm.description

         queryParams =
           UrlBuilder.absolute ["list"]
                    [ UrlBuilder.string "tags" tags, UrlBuilder.string "description" description]

         queryUrl = "http://localhost:5000" ++ queryParams
      in
          ( model
          , Http.get
              { url = queryUrl
              , expect = Http.expectJson GotAnswers answersDecoder
              }
          )

    SetSearchTags tags ->
      let
          searchForm =
            model.searchForm

          updatedSearchForm =
            { searchForm | tags = tags }
      in
          ( { model | searchForm = updatedSearchForm }
          , Cmd.none
          )


    SetSearchDesc desc ->
        let
            searchForm =
              model.searchForm

            updatedSearchForm =
              { searchForm | description = desc }

        in
            ( { model | searchForm = updatedSearchForm }
            , Cmd.none
            )



subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ consumeAudio UploadAnswer ]

{-| View Stuff -}
view : Model -> { title : String, body : Html Msg }
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
      { title = "Answers ~ Dashboard", body = views }

viewAnswerDashBoard : Model -> Html Msg
viewAnswerDashBoard model =
  div [ class "container" ]
      [ viewQuestions model
      , div [ id "send-col"]
            [ button [ id "send", onClick SendAnswer]
                     [ text "Send!" ]
            ]
      , viewAnswersSection model
      ]


viewAnswer : Answer -> Html Msg
viewAnswer ans =
  tr []
     [ td [ class "td-check"]
          [ input [ type_ "radio", value ans.audio_url, name "answer_audio", onInput AnswerSelected] [] ]
     , td [] [ text ans.description]
     , td [] [ text ans.tags]
     , td []
          [ audio [ controls True ]
                  [ source [ src ("http://localhost:5000/" ++ ans.audio_url) ] [] ]
          ]
     ]

viewAnswerList : Model -> Html Msg
viewAnswerList model =
  let
     tags =
      model.searchForm.tags

     desc =
      model.searchForm.description
  in
  div [ ]
      [ h2 [] [ text "Answers"]
      , div [ class "search-form"]
            [ label [ ] [ text "Search" ]
            , div []
                  [ label [] [ text "tags" ]
                  , input [ type_ "text", value tags, onInput SetSearchTags] []
                  ]
            , div []
                  [ label [] [ text "description" ]
                  , input [ type_ "text", value desc, onInput SetSearchDesc] []
                  ]
            , button [ onClick FilterAnswers] [ text "Search" ]
            ]
      , table []
              [ thead []
                      [ tr []
                           [ th [] [ text "Send" ]
                           , th [] [ text "Description"]
                           , th [] [ text "Answer" ]
                           ]
                      ]
              , tbody []
                      (List.map viewAnswer model.answers)
              ]
      ]


viewAnswersSection : Model -> Html Msg
viewAnswersSection model =
  div [ id "answers"]
    [ div [ id "add-answer" ]
           [ h3 [] [ text "Add Answer:"]
           , div []
                 [ label [ for "description" ] [ text "Description" ]
                 , textarea [ cols 80, rows 4, onInput SetDescription ]  []
                 ]
           , div []
                 [ label [ for "tags" ] [ text "Tags" ]
                 , input [ type_ "text", onInput SetTags ] []
                 ]
           , viewRecordingForm model
           , div []
                 [ button [ onClick SendToServer ] [ text "Upload Answer" ] ]
           ]
    , viewAnswerList model
    ]

viewRecordingForm : Model -> Html Msg
viewRecordingForm { answerForm } =
  let
      status =
        answerForm.status

      viewRecordingSection =
        case status of
          NoData ->
            text ""

          Recording ->
            div [ class "recording-status" ]
              [ p [] [text "red circle while recording"]
              , button [ onClick StopRecording] [ text "Stop" ]
              ]
          _ ->
            text ""

      uploadButton =
        case status of
         NoData ->
           button [ onClick WavRequested ] [ text "Upload" ]

         _ ->
            text ""


      recordButton =
        case status of
          NoData ->
            button [ onClick StartRecording ] [ text "Record" ]

          _ ->
            text ""

      statusText =
        case status of
          NoData ->
            "No Audio Supplied"

          Recording ->
            "Recording Audio"

          Selecting ->
            "Selecting Audio"

          Selected ->
            "Audio Selected"

          Recorded ->
            "Audio Recorded"

  in
      div [ id "answers"]
      [ p [] [ text statusText  ]
      , div [ class "file-form"]
            [ recordButton
            , uploadButton
            ]
      , viewRecordingSection
      , case answerForm.audio_url of
          Nothing ->
            text ""

          Just ar ->
            ar
            |>  viewSuppliedAudio
      ]

viewSuppliedAudio : String -> Html Msg
viewSuppliedAudio audioresource =
      div []
          [ p [] [ text "Audio Supplied" ]
          , audio [ controls True ]
                  [ source [ src audioresource ] [] ]
          , button [ onClick ClearAudio ] [ text "Remove" ]
          ]




viewQuestion : List Int -> Question -> Html Msg
viewQuestion ids ques =
  let
      stringId =
        ques.q_id
        |> String.fromInt

      isChecked =
        ids
        |> List.member ques.q_id


  in
         tr []
            [ td [ class "td-check"]
                 [ input [type_ "checkbox", value stringId, onInput QuestionChecked
                 , if isChecked then
                    checked True
                   else
                     checked False
                   ] [] ]
            , td [ ] [ text ques.q_meta]
            , td [ ] [ text stringId ]
            , td [ ] [ audio [ controls True]
                         [ source [src ("http://localhost:5000/" ++ ques.q_audio), type_ "audio/wav" ] [] ]
                     ]
            ]


viewQuestions : Model -> Html Msg
viewQuestions { sendForm, questions } =
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
                (List.map (\q -> viewQuestion sendForm.question_ids q) questions)
        ]
     ]



{-| internal types and decoders -}

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


toSession : Model -> Session
toSession { session } =
  session



port startRecording : () -> Cmd msg

port stopRecording : () -> Cmd msg

port consumeAudio : (String -> msg) -> Sub msg
