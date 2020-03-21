module Profile exposing (AgronomistPhone, Profile, decoder, encode, phoneToString)

import Agronomist.Cred as Cred exposing (Cred)
import Agronomistname exposing (Agronomistname)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


type AgronomistPhone
    = AgronomistPhone String


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
    Decode.map AgronomistPhone Decode.string


decoder : Decoder Profile
decoder =
    Decode.succeed Profile
        |> required "phone" phoneDecoder
