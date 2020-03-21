port module Page.AnswerDashboard exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Base64
import Bytes exposing (Bytes)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, audio, button, div,  h2, h3, input, label, p, source, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (checked, class, cols, controls, for, id, name, rows, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (bytesPart, filePart, jsonBody, multipartBody, stringPart)
import Json.Decode as JD exposing (Decoder, at, int, map3, map4, string)
import Json.Encode as Encode
import Session exposing (Session)
import Task
import Url.Builder as UrlBuilder


type alias Model =
    { answerForm : AnswerForm
    , sendForm : SendForm
    , searchForm : SearchForm
    , reassignForm : ReassignForm
    , answers : List Answer
    , questions : List Question
    , session : Session
    }


type AnswerFormStatus
    = NoData
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


type alias ReassignForm =
    { src : String
    , dest : String
    , questions : List Int
    }


type AudioResource
    = F File
    | R Bytes


init : Maybe String -> Session -> ( Model, Cmd Msg )
init maybePhone session =
    case maybePhone of
        Just p ->
            let
                profileData =
                    { phone = p }

                loginData =
                    { token = p
                    , user = profileData
                    }
            in
            ( initModel session, Session.login loginData )

        Nothing ->
            ( initModel session
            , Cmd.batch
                [ getAnswers
                , getQuestions session
                ]
            )


initModel : Session -> Model
initModel sess =
    let
        mod =
            { session = sess
            , answerForm = initAnswerForm
            , sendForm = initSendForm
            , searchForm = initSearchForm
            , reassignForm = initReassignForm
            , answers = []
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


initReassignForm : ReassignForm
initReassignForm =
    { src = ""
    , dest = ""
    , questions = []
    }


type Msg
    = StartRecording
    | StopRecording
    | UploadAnswer String
    | WavRequested
    | WavLoaded File
    | SendToServer
    | GotUploadResponse (Result Http.Error ())
    | GotAnswers (Result Http.Error (List Answer))
    | GotQuestions (Result Http.Error (List Question))
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
    | SetReassignee String
    | Reassign
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
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
            ( { model | answerForm = updatedAnswerForm }, start )

        StopRecording ->
            let
                stop =
                    stopRecording ()

                answerForm =
                    model.answerForm

                updatedAnswerForm =
                    { answerForm | status = Recorded }
            in
            ( { model | answerForm = updatedAnswerForm }, stop )

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
                            { answerForm | audio = Just (R byts), audio_url = audio_url }

                        Nothing ->
                            answerForm
            in
            ( { model | answerForm = updatedForm }, Cmd.none )

        WavRequested ->
            let
                answerForm =
                    model.answerForm

                updatedAnswerForm =
                    { answerForm | status = Selecting }
            in
            ( { model | answerForm = updatedAnswerForm }
            , Select.file [ "audio/wav" ] WavLoaded
            )

        WavLoaded file ->
            let
                answerForm =
                    model.answerForm

                updatedForm =
                    { answerForm | audio = Just (F file), status = Selected }
            in
            ( { model | answerForm = updatedForm }
            , Task.perform SetAudioUrl (File.toUrl file)
            )

        SendToServer ->
            case model.answerForm.audio of
                Nothing ->
                    ( model, Cmd.none )

                {- Should be covered using error handling -}
                Just ar ->
                    let
                        files =
                            case ar of
                                F f ->
                                    filePart "audio" f

                                R r ->
                                    bytesPart "audio" "audio/*" r

                        ts =
                            model.answerForm.tags
                                |> String.split ","
                                |> Encode.list Encode.string
                                |> Encode.encode 0

                        body =
                            multipartBody
                                [ files
                                , stringPart "description" model.answerForm.desc
                                , stringPart "tags" ts
                                ]
                    in
                    ( model
                    , Http.post
                        { url = "http://localhost:5000/add"
                        , body = body
                        , expect = Http.expectWhatever GotUploadResponse
                        }
                    )

        GotUploadResponse res ->
            case res of
                Ok _ ->
                    ( { model | answerForm = initAnswerForm }
                    , getAnswers
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotAnswers res ->
            case res of
                Ok a ->
                    ( { model | answers = a }, Cmd.none )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        GotQuestions res ->
            case res of
                Ok q ->
                    ( { model | questions = q }
                    , Cmd.none
                    )

                Err _ ->
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
            ( { model | answerForm = updatedForm }
            , Cmd.none
            )

        SetDescription desc ->
            let
                answerForm =
                    model.answerForm

                updatedForm =
                    { answerForm | desc = desc }
            in
            ( { model | answerForm = updatedForm }
            , Cmd.none
            )

        SendAnswer ->
            let
                encodedList =
                    model.sendForm.question_ids
                        |> Encode.list Encode.int

                reqBody =
                    Encode.object
                        [ ( "queue_owner", Encode.string model.sendForm.queue_owner )
                        , ( "answer_audio", Encode.string model.sendForm.answer_audio )
                        , ( "question_ids", encodedList )
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
            ( model, sendAnswer )

        AnswerSelected ans ->
            let
                sendForm =
                    model.sendForm

                updatedSendForm =
                    { sendForm | answer_audio = ans }
            in
            ( { model | sendForm = updatedSendForm }, Cmd.none )

        QuestionChecked stringId ->
            let
                sendForm =
                    model.sendForm

                reassignForm =
                    model.reassignForm

                q_ids =
                    sendForm.question_ids

                maybeId =
                    stringId
                        |> String.toInt

                updatedSendForm =
                    case maybeId of
                        Just id ->
                            if List.member id q_ids then
                                { sendForm | question_ids = q_ids |> List.filter (\x -> x /= id) }

                            else
                                { sendForm | question_ids = id :: q_ids }

                        Nothing ->
                            sendForm

                updatedReassignForm =
                    case maybeId of
                        Just id ->
                            if List.member id q_ids then
                                { reassignForm | questions = q_ids |> List.filter (\x -> x /= id) }

                            else
                                { reassignForm | questions = id :: q_ids }

                        Nothing ->
                            reassignForm
            in
            ( { model | sendForm = updatedSendForm, reassignForm = updatedReassignForm }, Cmd.none )

        AnswerSent res ->
            case res of
                Ok qs ->
                    ( { model | questions = qs, sendForm = initSendForm }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        SetAudioUrl url ->
            let
                answerForm =
                    model.answerForm

                updatedAnswerForm =
                    { answerForm | audio_url = Just url }
            in
            ( { model | answerForm = updatedAnswerForm }
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
                        |> String.split ","

                description =
                    model.searchForm.description

                body =
                    Encode.object
                        [ ( "description", Encode.string description )
                        , ( "tags", Encode.list Encode.string tags )
                        ]
                        |> jsonBody

                queryUrl =
                    "http://localhost:5000/list"
            in
            ( model
            , Http.post
                { url = queryUrl
                , body = body
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

        SetReassignee string ->
            let
                reassignForm =
                    model.reassignForm

                updatedReassignForm =
                    { reassignForm | dest = string }
            in
            ( { model | reassignForm = updatedReassignForm }, Cmd.none )

        Reassign ->
            let
                form =
                    model.reassignForm

                phone =
                    Session.phoneString model.session
            in
            if String.isEmpty form.dest then
                ( model, Cmd.none )

            else
                let
                    body =
                        Encode.object
                            [ ( "src_queue_owner", Encode.string phone )
                            , ( "dest_queue_owner", Encode.string form.dest )
                            , ( "question_ids", Encode.list Encode.int form.questions )
                            ]
                            |> jsonBody
                in
                ( model
                , Http.post
                    { url = "http://localhost:5000/reassign"
                    , body = body
                    , expect = Http.expectJson GotAnswers answersDecoder
                    }
                )

        GotSession sess ->
            ( model
            , Cmd.batch
                [ getAnswers
                , getQuestions sess
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ consumeAudio UploadAnswer
        , Session.changes GotSession (Session.navKey model.session)
        ]


{-| View Stuff
-}
view : Model -> { title : String, body : Html Msg }
view model =
    let
        views =
            div [ class "container" ]
                [ viewQuestions model
                , div [ id "send-col" ]
                    [ button [ id "send", onClick SendAnswer ]
                        [ text "Send!" ]
                    , div []
                        [ label [] [ text "Reassignee:" ]
                        , input [ type_ "text", onInput SetReassignee ] []
                        , button [ onClick Reassign ] [ text "Reassign" ]
                        ]
                    ]
                , viewAnswersSection model
                ]
    in
    { title = "Answers ~ Dashboard", body = views }


viewAnswer : Answer -> Html Msg
viewAnswer ans =
    tr []
        [ td [ class "td-check" ]
            [ input [ type_ "radio", value ans.audio_url, name "answer_audio", onInput AnswerSelected ] [] ]
        , td [] [ text ans.description ]
        , td [] [ text (String.join "," ans.tags) ]
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
    div []
        [ h2 [] [ text "Answers" ]
        , div [ class "search-form" ]
            [ label [] [ text "Search" ]
            , div []
                [ label [] [ text "tags" ]
                , input [ type_ "text", value tags, onInput SetSearchTags ] []
                ]
            , div []
                [ label [] [ text "description" ]
                , input [ type_ "text", value desc, onInput SetSearchDesc ] []
                ]
            , button [ onClick FilterAnswers ] [ text "Search" ]
            ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Send" ]
                    , th [] [ text "Description" ]
                    , th [] [ text "Answer" ]
                    ]
                ]
            , tbody []
                (List.map viewAnswer model.answers)
            ]
        ]


viewAnswersSection : Model -> Html Msg
viewAnswersSection model =
    div [ id "answers" ]
        [ div [ id "add-answer" ]
            [ h3 [] [ text "Add Answer:" ]
            , div []
                [ label [ for "description" ] [ text "Description" ]
                , textarea [ cols 80, rows 4, onInput SetDescription ] []
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
                        [ p [] [ text "red circle while recording" ]
                        , button [ onClick StopRecording ] [ text "Stop" ]
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
    div [ id "answers" ]
        [ p [] [ text statusText ]
        , div [ class "file-form" ]
            [ recordButton
            , uploadButton
            ]
        , viewRecordingSection
        , case answerForm.audio_url of
            Nothing ->
                text ""

            Just ar ->
                ar
                    |> viewSuppliedAudio
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
        [ td [ class "td-check" ]
            [ input
                [ type_ "checkbox"
                , value stringId
                , onInput QuestionChecked
                , if isChecked then
                    checked True

                  else
                    checked False
                ]
                []
            ]
        , td [] [ text ques.q_meta ]
        , td [] [ text stringId ]
        , td []
            [ audio [ controls True ]
                [ source [ src ("http://localhost:5000/" ++ ques.q_audio), type_ "audio/wav" ] [] ]
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
                    , th []
                        [ text "Phone" ]
                    , th []
                        [ text "Received" ]
                    ]
                ]
            , tbody []
                (List.map (\q -> viewQuestion sendForm.question_ids q) questions)
            ]
        ]


{-| internal types and decoders
-}
type alias Question =
    { q_audio : String
    , q_id : Int
    , q_meta : String
    }


questionDecoder : Decoder Question
questionDecoder =
    map3 Question
        (at [ "q_audio" ] string)
        (at [ "q_id" ] int)
        (at [ "q_meta" ] string)


questionsDecoder : Decoder (List Question)
questionsDecoder =
    JD.list questionDecoder


getQuestions : Session -> Cmd Msg
getQuestions sesh =
    let
        phone =
            sesh
                |> Session.phoneString

        link =
            UrlBuilder.crossOrigin "http://localhost:5000/list-questions" [ phone ] []
    in
    Http.get
        { url = link
        , expect = Http.expectJson GotQuestions questionsDecoder
        }


type alias Answer =
    { audio_url : String
    , description : String
    , fname : String
    , tags : List String
    }


answerDecoder : Decoder Answer
answerDecoder =
    map4 Answer
        (at [ "audio_url" ] string)
        (at [ "description" ] string)
        (at [ "fname" ] string)
        (at [ "tags" ] (JD.list JD.string))


getAnswers : Cmd Msg
getAnswers =
    let
        body =
            Encode.object
                [ ( "description", Encode.string "" )
                , ( "tags", Encode.list Encode.string [] )
                ]
                |> jsonBody
    in
    Http.post
        { url = "http://localhost:5000/list"
        , body = body
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
