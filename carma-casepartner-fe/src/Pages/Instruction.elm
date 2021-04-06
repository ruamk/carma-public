module Pages.Instruction exposing (Flags, Model, Msg, page)

import Bootstrap.Dropdown as Dropdown
import Bootstrap.Navbar as Navbar
import Generated.Route as Route
import Global
import Html
import Html.Attributes as A
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Ui



type alias Flags =
    ()


type alias Model =
    { messageToast : MessageToast Msg
    , navbarState : Navbar.State
    , usermenuState : Dropdown.State
    }


type Msg
    = GoToCases
    | GoToLogout
    | GoToSearchCases
    | NavbarMsg Navbar.State
    | UsermenuMsg Dropdown.State
    | Settings
    | Instruction
    | UpdateCustomMessageToast (MessageToast Msg)


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
    ( { messageToast =
            MessageToast.initWithConfig UpdateCustomMessageToast
                { delayInMs = 2000
                , toastsToShow = 10
                }
      , navbarState = navbarState
      , usermenuState = Dropdown.initialState
      }
    , navbarCmd
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of 
        GoToCases ->
            ( model
            , Cmd.none
            , Global.navigate Route.Services
            )

        GoToLogout ->
            ( model
            , Cmd.none
            , Global.logout
            )

        GoToSearchCases ->
            ( model
            , Cmd.none
            , Global.navigate Route.Search
            )

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.none
            , Cmd.none
            )

        UsermenuMsg state ->
            ( { model | usermenuState = state }
            , Cmd.none
            , Cmd.none
            )

        Settings ->
            ( model
            , Cmd.none
            , Global.settings
            )

        Instruction ->
            ( model
            , Cmd.none
            , Global.instruction
            )
        
        UpdateCustomMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }
            , Cmd.none
            , Cmd.none
            )

subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Instruction"
    , body =
        [ Ui.page
            { navbarMsg = NavbarMsg
            , navbarState = model.navbarState
            , logoutMsg = GoToLogout
            , settingsMsg = Nothing
            , usermenuMsg = UsermenuMsg
            , usermenuState = model.usermenuState
            , username = global.username
            , buttons =
                [ ( False, GoToCases, "Текущие заявки" )
                , ( False, GoToSearchCases, "Поиск заявок" )
                , ( False, Settings, "Настройки" )
                , ( True, Instruction, "Инструкция" )
                ]
            }
          <|
            Html.iframe 
                [ A.src "/book/index.html"
                , A.attribute "frameborder" "0" 
                , A.attribute "marginheight" "0" 
                , A.attribute "marginwidth" "0" 
                , A.attribute "width" "100%" 
                , A.attribute "height" "100%" 
                , A.attribute "scrolling" "auto"
                , A.style "position" "fixed"
                ] 
                []
        ]
    }

