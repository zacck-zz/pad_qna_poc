module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (Html)


type Page
    = Other
    | Landing
    | AnswerDashboard


view : Page -> { title : String, body : Html msg } -> Document msg
view _ { title, body } =
    { title = title
    , body = [body]
    }
