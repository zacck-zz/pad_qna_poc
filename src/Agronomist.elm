module Agronomist exposing(Agronomist, cred, decode, profile, phone)

{-| The logged in Agronomist on the Dashboard -}

import Agronomist.Cred as Cred exposing(..)
import Profile exposing (..)
import Agronomistname exposing (..)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)



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
        (Decode.field "phone" Cred.decoder)
        (Decode.field "profile" Profile.decoder)
        |> Decode.map Agronomist
