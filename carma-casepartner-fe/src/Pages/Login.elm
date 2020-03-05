module Pages.Login exposing (Model, Msg, page)

import Api
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input exposing (button, currentPassword, labelLeft, username)
import Generated.Params as Params
import Generated.Routes as Routes exposing (routes)
import Global
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


view : PageContext -> Model -> Element Msg
view _ model =
    column
        [ centerX
        , centerY
        , spacing 16
        ]
        [ el [] <|
            image []
                { src = "/logo.png"
                , description = ""
                }
        , el
            [ alignLeft
            , Font.size 24
            , Font.semiBold
            ]
          <|
            text "Вход в систему"
        , Ui.horizontalLine
        , el [ alignRight ] <|
            username [ Font.size 12 ]
                { label = labelLeft [ centerY ] <| text "Имя пользователя"
                , text = model.name
                , onChange = Name
                , placeholder = Nothing
                }
        , el [ alignRight ] <|
            currentPassword [ Font.size 12 ]
                { label = labelLeft [ centerY ] <| text "Пароль"
                , text = model.password
                , onChange = Password
                , placeholder = Nothing
                , show = False
                }
        , el [] none
        , el [ centerX ] <| Ui.button ( "Войти", Just TryLogin )
        , el [ centerX ]
            (case model.errorMessage of
                Nothing ->
                    el [] none

                Just errorMessage ->
                    el
                        [ Font.color Ui.colors.red
                        , Font.size 12
                        ]
                    <|
                        text ("Ошибка: " ++ errorMessage)
            )
        ]
