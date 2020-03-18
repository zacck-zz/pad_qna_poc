port module Session exposing(Session, decode, navKey, login, changes, cred, phoneString, viewer, logout)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Agronomist exposing (..)
import Agronomist.Cred as Cred exposing (..)
import Profile exposing (..)


type Session
    = LoggedIn Nav.Key Agronomist
    | Guest Nav.Key

phoneString : Session -> String
phoneString session =
  case session of
    Guest key ->
      "master"


    LoggedIn _ agronomist ->
      agronomist
      |> phone
      |> phoneToString


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


decode : Nav.Key -> Decode.Value -> Session
decode key value =
    let
        decoded_session =
            Decode.decodeValue Decode.string value
                |> Result.andThen (Decode.decodeString Agronomist.decode)
                |> Result.toMaybe
    in
    case decoded_session of
        Just agronomist ->
            LoggedIn key agronomist

        Nothing ->
            Guest key


viewer : Session -> Maybe Agronomist
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (Agronomist.cred val)

        Guest _ ->
            Nothing

changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    sessionChanged (\val -> toMsg (decode key val))


logout : Cmd msg
logout =
    storeSession Nothing


login : { token : String, user : Profile } -> Cmd msg
login { token, user } =
    let
        session_value =
            Encode.object
                [ ( "token", Cred.encodeToken token )
                , ( "profile"
                  , Encode.object
                        [ ( "phone", Encode.string (phoneToString user.phone) ) ]
                  )
                ]

        encoded_s =
            Encode.encode 0 session_value
    in
    encoded_s
        |> Just
        |> storeSession

port storeSession : Maybe String -> Cmd msg


port sessionChanged : (Decode.Value -> msg) -> Sub msg


