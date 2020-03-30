module Pages.NotFound exposing (Model, Msg, page)

import Generated.Params as Params
import Generated.Routes as Routes exposing (routes)
import Html exposing (..)
import Spa.Page
import Ui
import Utils.Spa exposing (Page)


type alias Model =
    ()


type alias Msg =
    Never


page : Page Params.NotFound Model Msg model msg appMsg
page =
    Spa.Page.static
        { title = always "not found | elm-spa"
        , view = always view
        }



-- VIEW


view : Html Msg
view =
    div [] [ text "404 is life." ]
