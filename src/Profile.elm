module Profile exposing (Profile, AgronomistPhone, encode, decoder, phoneToString)

import Agronomistname exposing (Agronomistname)
import Agronomist.Cred as Cred exposing (Cred)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Decode.Pipeline exposing (custom, required)


type AgronomistPhone =
  AgronomistPhone String

type alias Profile =
    { phone : AgronomistPhone }


phoneToString : AgronomistPhone -> String
phoneToString (AgronomistPhone p) =
  p


encode : Profile -> Value
encode info =
    Encode.object
        [ ( "phone", Encode.string (phoneToString info.phone) ) ]


phoneDecoder : Decoder AgronomistPhone
phoneDecoder =
    Decode.map AgronomistPhone (Decode.string)

decoder : Decoder Profile
decoder =
    Decode.succeed Profile
        |> required "phone" phoneDecoder
