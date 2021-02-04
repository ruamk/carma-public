module Pages.Top exposing (Flags, Model, Msg, page)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Flex as Flex
import Generated.Route as Route
import Global
import Html exposing (h1, text)
import Html.Attributes exposing (attribute, style)
import Page exposing (Document, Page)
import Time
import Url


type alias Flags =
    ()


type alias Model =
    { counter : Int
    }


type Msg
    = Tick Time.Posix


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init model _ =
    let
        url : Url.Url
        url =
            model.url
    in
    ( { counter = 0 }
    , Cmd.none
    , Global.navigate <|
        if model.route /= "" then
            case Route.fromUrl { url | path = model.route } of
                Just r ->
                    r

                Nothing ->
                    Route.Login

        else
            Route.Login
    )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Time.every 1000 Tick


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        Tick _ ->
            ( { model | counter = model.counter - 1 }
            , Cmd.none
            , if model.counter > 1 then
                Cmd.none

              else
                Global.navigate Route.Login
            )


view : Global.Model -> Model -> Document Msg
view _ model =
    { title = "Top"
    , body =
        [ Grid.row [ Row.centerXs, Row.attrs [ style "height" "100vh" ] ]
            [ Grid.col [ Col.sm2, Col.attrs [ Flex.alignSelfCenter ] ]
                [ h1 [ attribute "class" "text-center" ]
                    [ text <| String.fromInt model.counter ]
                ]
            ]
        ]
    }
