module Pages.Map exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , br
        , div
        , text
        )
import Html.Attributes as A
import Html.Events exposing (onClick)
import Http
import LeafletMap
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Types
import Ui



type alias Flags =
    ()


type alias Model =
    { navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , messageToast : MessageToast Msg
    , drivers : List Types.DriverLocation
    , panelState : Bool
    }


type Msg
    = NavbarMsg Navbar.State
    | Logout
    | Services
    | Settings
    | DriversMap
    | UpdateCustomMessageToast (MessageToast Msg)
    | UsermenuMsg Dropdown.State
    | PanelButton Bool
    | GotDriverLocations (Result Http.Error (List Types.DriverLocation))


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
      , drivers = []
      , panelState = False
      }
    , Cmd.batch 
        [ navbarCmd
        , Api.getDriverLocations GotDriverLocations
        ]
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

        DriversMap ->
            ( model
            , Cmd.none
            , Cmd.none
            )

        PanelButton b ->
            ( { model | panelState = b }
            , Cmd.none
            , Cmd.none
            )
        
        GotDriverLocations result ->
            case result of 
                Err error ->
                    ( model
                    , Cmd.none
                    , Cmd.none
                    )
                
                Ok drivers ->
                    ( { model | drivers = drivers }
                    , Cmd.none
                    , Cmd.none
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
                , ( True, DriversMap, "Карта водителей"  )
                ]
            }
          <|
            div []
                [ viewContent model
                , br [] []
                , div []
                    [ model.messageToast
                        |> MessageToast.overwriteContainerAttributes
                            [ A.style "top" "60px"
                            , A.style "bottom" "auto"
                            , A.style "right" "20px"
                            ]
                        |> MessageToast.view
                    ]
                ]
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    let
        desktop =
            Grid.row []
                [ Grid.col [ Col.sm3 ]
                    [ viewDriversTable model.drivers
                    ]
                , Grid.col [ Col.sm9 ]
                    [ viewMap model
                    ]
                ]

        mobile =
            div []
                [ if model.panelState then
                    viewDriversTable model.drivers

                  else
                    viewMap model
                , viewTableFoldingButton model
                ]
    in
    div []
        [ div [ A.class "d-none d-xl-block" ] [ desktop ]
        , div [ A.class "d-block d-xl-none" ] [ mobile ]
        ]


viewMap : Model -> Html Msg
viewMap model =
    let
        driverMarker driver =
            ( "marker"
            , LeafletMap.marker
                [ LeafletMap.iconUrl "driver.svg"
                , LeafletMap.iconHeight 32
                , LeafletMap.iconWidth 32
                , LeafletMap.latitude driver.latitude
                , LeafletMap.longitude driver.longitude
                ]
                [ text driver.name
                ]
            )
    in
    LeafletMap.view
        [ A.style "width" "100%"
        , A.style "height" "100vh"
        , A.style "position" "relative"
        , A.style "display" "block"
        , LeafletMap.latitude 55.758222
        , LeafletMap.longitude 37.622142
        , LeafletMap.scale 13
        , LeafletMap.tileLayer "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        ]
        (List.map driverMarker model.drivers)


viewDriversTable : List Types.DriverLocation -> Html Msg
viewDriversTable drivers =
    let
        options =
            []

        head =
            Table.simpleThead
                [ Table.th [] [ text "Иконка" ]
                , Table.th [] [ text "Имя" ]
                ]

        body =
            Table.tbody [] (List.map driverTableRow drivers)

        driverTableRow driver =
            Table.tr
                []
                [ Table.td [] [ viewIcon "driver.svg" ]
                , Table.td [] [ text driver.name ]
                ]

        viewIcon src =
            Html.img
                [ A.src src
                , A.style "width" "32px"
                , A.style "height" "32px"
                ]
                []
    in
    Table.table
        { options = options
        , thead = head
        , tbody = body
        }


viewTableFoldingButton : Model -> Html Msg
viewTableFoldingButton model =
    let
        svgList =
            Html.img [ A.src "list.svg" ] []

        svgMap =
            Html.img [ A.src "map.svg" ] []

        attrs =
            [ A.style "position" "fixed"
            , A.style "right" "2em"
            , A.style "bottom" "2em"
            , A.style "width" "50px"
            , A.style "height" "36px"
            , A.style "background-color" "rgb(18, 147, 216)"
            , A.style "color" "white"
            , A.style "font-family" "monospace"
            , A.style "pointer-events" "auto"
            , A.style "z-index" "2147483647"
            , A.style "display" "flex"
            , A.style "justify-content" "center"
            , A.style "align-items" "center"
            , A.style "cursor" "pointer"
            , onClick (PanelButton <| not model.panelState)
            ]
    in
    div attrs
        [ if model.panelState then
            svgMap

          else
            svgList
        ]
