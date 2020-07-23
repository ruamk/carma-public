module Pages.Login exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Flex as Flex
import Generated.Route as Route
import Global
import Html
    exposing
        ( div
        , h3
        , hr
        , img
        , text
        )
import Html.Attributes
    exposing
        ( for
        , src
        , style
        )
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    { name : String
    , password : String
    , errorMessage : Maybe String
    }


type Msg
    = Name String
    | Password String
    | TryLogin
    | LoginStatus (Result Http.Error Int)


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    ( { name = ""
      , password = ""
      , errorMessage = Nothing
      }
    , Cmd.none
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
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
                code : Int
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
                    , Cmd.batch
                        [ Global.saveUsername model.name
                        , Global.navigate Route.Cases
                        ]
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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


view : Global.Model -> Model -> Document Msg
view _ model =
    { title = "Вход в систему"
    , body =
        [ Grid.row [ Row.centerXs, Row.attrs [ style "height" "100vh" ] ]
            [ Grid.col [ Col.sm2, Col.attrs [ Flex.alignSelfCenter ] ]
                [ div []
                    [ img [ src "/logo.png" ] [] ]
                , div [ style "height" "2rem" ] []
                , h3 [] [ text "Вход в систему" ]
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
        ]
    }
