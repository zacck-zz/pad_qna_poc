module Page.Blank exposing (view)

import Html exposing (Html)

view : { title : String, body : Html msg }
view =
    { title = ""
    , body = Html.text ""
    }
