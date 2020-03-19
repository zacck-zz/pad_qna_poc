module Page.NotFound exposing (view)

import Html exposing (Html, div, text, main_)

-- VIEW

view : { title : String, body : Html msg  }
view =
    { title = "Page Not Found"
    , body = Html.text "Oops Page Not Found"
    }

