module Pages.Login exposing (Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Size as Size
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Flex as Flex
import Generated.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Ports
import Spa.Page
import Ui
import Utils.Spa exposing (Page, PageContext)


page : Page Params.Login Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Вход в систему"
        , init = init
        , update = update
        , view = view
        , subscriptions = always subscriptions
        }



-- INIT


type alias Model =
    { name : String
    , password : String
    , errorMessage : Maybe String
    }


init : PageContext -> Params.Login -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( { name = ""
      , password = ""
      , errorMessage = Nothing
      }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = Name String
    | Password String
    | TryLogin
    | LoginStatus (Result Http.Error Int)


update : PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update commands msg model =
    case msg of
        Name name ->
            ( { model | name = name }
            , Cmd.none
            , Cmd.none
            )

        Password password ->
            ( { model | password = password }
            , Cmd.none
            , Cmd.none
            )

        TryLogin ->
            if model.name == "" then
                ( { model | errorMessage = Just "укажите имя пользователя!" }
                , Cmd.none
                , Cmd.none
                )

            else if model.password == "" then
                ( { model | errorMessage = Just "укажите пароль!" }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model | errorMessage = Nothing }
                , Api.login model.name model.password LoginStatus
                , Cmd.none
                )

        LoginStatus result ->
            let
                code =
                    case result of
                        Ok value ->
                            value

                        Err value ->
                            500
            in
            case code of
                200 ->
                    ( { model | errorMessage = Nothing }
                    , Cmd.none
                    , Spa.Page.send <| Global.Cases model.name
                    )

                401 ->
                    ( { model | errorMessage = Just "Неверно указано имя пользователя или пароль!" }
                    , Cmd.none
                    , Cmd.none
                    )

                _ ->
                    ( { model | errorMessage = Just "Ошибка сети!" }
                    , Cmd.none
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : PageContext -> Model -> Html Msg
view _ model =
    Grid.row [ Row.centerXs, Row.attrs [ style "height" "100vh"] ]
        [ Grid.col [ Col.sm2, Col.attrs [ Flex.alignSelfCenter ] ]
            [ div []
                [ img
                    [ src "/logo.png"
                    ]
                    []
                ]
            , div [style "height" "2rem"] []
            , h3
                []
                [ text "Вход в систему"
                ]
            , hr [] []
            , Form.form []
                [ Form.group []
                    [ Form.label [ for "name" ] [ text "Имя пользователя" ]
                    , Input.text [ Input.id "name", Input.value model.name, Input.attrs [ onInput Name ] ]
                    ]
                , Form.group []
                    [ Form.label [ for "password" ] [ text "Пароль" ]
                    , Input.password [ Input.id "password", Input.value model.password, Input.attrs [ onInput Password ] ]
                    ]
                , Form.row [ Row.rightSm ]
                    [ Form.col []
                        [ div []
                            [ text <|
                                case model.errorMessage of
                                    Nothing ->
                                        ""

                                    Just errorMessage ->
                                        "Ошибка: " ++ errorMessage
                            ]
                        ]
                    ]
                ]
            , Button.button [ Button.primary, Button.attrs [ onClick TryLogin ] ]
                [ text "Войти" ]
            ]
        ]
