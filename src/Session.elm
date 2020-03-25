port module Session exposing (Session, changes, cred, decode, login, logout, navKey, phoneString, viewer)

import Agronomist exposing (Agronomist, phone)
import Agronomist.Cred as Cred exposing (Cred)
import Browser.Navigation as Nav
import Json.Decode as Decode
import Json.Encode as Encode
import Profile exposing  (phoneToString)


type Session
    = LoggedIn Nav.Key Agronomist
    | Guest Nav.Key


phoneString : Session -> String
phoneString session =
    case session of
        Guest _ ->
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


login : { token : String, user : { phone : String } } -> Cmd msg
login { token, user } =
    let
        session_value =
            Encode.object
                [ ( "token", Cred.encodeToken token )
                , ( "profile"
                  , Encode.object
                        [ ( "phone", Encode.string user.phone ) ]
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
