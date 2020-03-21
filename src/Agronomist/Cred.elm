module Agronomist.Cred exposing (Cred, decoder, encodeToken)

{-| Auth Cred for the currently logged in player
-}

import Agronomistname exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


type Cred
    = Cred String



-- name


encodeToken : String -> Value
encodeToken str =
    Encode.string str


decoder : Decoder Cred
decoder =
    Decode.map Cred Decode.string
