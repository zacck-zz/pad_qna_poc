module Page.NotFound exposing (view)

import Html exposing (Html,  text)



-- VIEW


view : { title : String, body : Html msg }
view =
    { title = "Page Not Found"
    , body = Html.text "Oops Page Not Found"
    }
