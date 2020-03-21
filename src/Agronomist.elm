module Agronomist exposing (Agronomist, cred, decode, phone, profile)

{-| The logged in Agronomist on the Dashboard
-}

import Agronomist.Cred as Cred exposing (..)
import Agronomistname exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (..)



-- Types


type Agronomist
    = Agronomist Internals


type alias Internals =
    { cred : Cred
    , profile : Profile
    }



-- info


cred : Agronomist -> Cred
cred (Agronomist data) =
    data.cred


phone : Agronomist -> AgronomistPhone
phone (Agronomist data) =
    data.profile.phone


profile : Agronomist -> Profile
profile (Agronomist data) =
    data.profile


decode : Decoder Agronomist
decode =
    Decode.map2 Internals
        (Decode.field "token" Cred.decoder)
        (Decode.field "profile" Profile.decoder)
        |> Decode.map Agronomist
