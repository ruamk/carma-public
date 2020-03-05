module Pages.Top exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (currentPassword, labelLeft, username)
import Generated.Params as Params
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Spa.Page
import Ui
import Utils.Spa exposing (Page)


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "homepage"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    {}


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Element Msg
view model =
    column [] []
