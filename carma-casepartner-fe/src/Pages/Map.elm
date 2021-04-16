module Pages.Map exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Navbar as Navbar
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , br
        , div
        , h3
        , h4
        , text
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , style
        )
import Html.Events exposing (onClick)
import Http
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Types exposing (Dictionary, ServiceInfo)
import Ui


type alias Flags =
    ()


type alias Model =
    { navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , messageToast : MessageToast Msg
    }


type Msg
    = NavbarMsg Navbar.State
    | Logout
    | Services
    | Settings
    | UpdateCustomMessageToast (MessageToast Msg)
    | UsermenuMsg Dropdown.State


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
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState
      , usermenuState = Dropdown.initialState
      , messageToast =
            MessageToast.initWithConfig UpdateCustomMessageToast
                { delayInMs = 2000
                , toastsToShow = 10
                }
      }
    , navbarCmd
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        -- Entry point
        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.none
            , Cmd.none
            )

        Logout ->
            ( model
            , Cmd.none
            , Global.logout
            )

        UsermenuMsg state ->
            ( { model | usermenuState = state }
            , Cmd.none
            , Cmd.none
            )

        Services ->
            ( model
            , Cmd.none
            , Global.navigate Route.Services
            )

        UpdateCustomMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }
            , Cmd.none
            , Cmd.none
            )

        Settings ->
            ( model
            , Cmd.none
            , Global.settings
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Dropdown.subscriptions model.usermenuState UsermenuMsg
        ]


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Карта водителей"
    , body =
        [ Ui.page
            { navbarMsg = NavbarMsg
            , logoutMsg = Logout
            , settingsMsg = Just Settings
            , usermenuMsg = UsermenuMsg
            , navbarState = model.navbarState
            , usermenuState = model.usermenuState
            , username = global.username
            , buttons =
                [ ( False, Services, "Текущие заявки" )
                , ( False, NavbarMsg model.navbarState, "Поиск заявок" )
                , ( False, Settings, "Настройки" )
                ]
            }
          <|
            div []
                [ viewContent model
                , br [] []
                , div []
                    [ model.messageToast
                        |> MessageToast.overwriteContainerAttributes
                            [ style "top" "60px"
                            , style "bottom" "auto"
                            , style "right" "20px"
                            ]
                        |> MessageToast.view
                    ]
                ]
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div [] []
