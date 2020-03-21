module Agronomistname exposing (Agronomistname, decoder, encode, toHtml, toString)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser


type Agronomistname
    = Agronomistname String


toString : Agronomistname -> String
toString (Agronomistname playername) =
    playername


toHtml : Agronomistname -> Html msg
toHtml (Agronomistname name) =
    Html.text name


decoder : Decoder Agronomistname
decoder =
    Decode.map Agronomistname Decode.string


encode : Agronomistname -> Value
encode (Agronomistname playername) =
    Encode.string playername


urlParser : Url.Parser.Parser (Agronomistname -> a) a
urlParser =
    Url.Parser.custom "AGRONOMISTNAME" (\str -> Just (Agronomistname str))
