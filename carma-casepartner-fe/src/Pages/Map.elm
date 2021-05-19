module Pages.Map exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Checkbox as Checkbox
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
import Lib.LeafletMap as LeafletMap
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Time
import Types
import Ui
import Utils


updateLocationsRateSec : Float
updateLocationsRateSec =
    60


type alias Flags =
    ()


type alias Model =
    { navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , messageToast : MessageToast Msg
    , drivers : List Types.DriverLocation
    , hiddenDrivers : List Types.DriverLocation
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
    | UpdateLocationsTick Time.Posix
    | DriverVisibility Types.DriverLocation Bool


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
      , hiddenDrivers = []
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
    let
        showError : String -> Model
        showError e =
            let
                messageToast : MessageToast Msg
                messageToast =
                    model.messageToast
                        |> MessageToast.danger
                        |> MessageToast.withMessage e
            in
            { model | messageToast = messageToast }
    in
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
                    ( showError "Не могу получить список водителей от сервера"
                    , Cmd.none
                    , Cmd.none
                    )

                Ok drivers ->
                    ( { model | drivers = drivers }
                    , Cmd.none
                    , Cmd.none
                    )

        UpdateLocationsTick _ ->
            ( model
            , Api.getDriverLocations GotDriverLocations
            , Cmd.none
            )

        DriverVisibility driver visibility ->
            if visibility then
                ( { model
                    | hiddenDrivers = List.filter (\d -> d.id /= driver.id) model.hiddenDrivers
                  }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model
                    | hiddenDrivers = driver :: model.hiddenDrivers
                  }
                , Cmd.none
                , Cmd.none
                )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Dropdown.subscriptions model.usermenuState UsermenuMsg
        , Time.every (1000 * updateLocationsRateSec) UpdateLocationsTick
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
                , ( True, DriversMap, "Карта водителей" )
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
                            , A.style "z-index" "9999"
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
                    [ viewDriversTable model
                    ]
                , Grid.col [ Col.sm9 ]
                    [ viewMap model
                    ]
                ]

        mobile =
            div []
                [ if model.panelState then
                    viewDriversTable model

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
            let
                markerHTML =
                    Utils.viewColoredMarkerRawHTML 32 (unpackCarColor driver.carInfo)
            in
            LeafletMap.marker
                [ LeafletMap.iconHeight 32
                , LeafletMap.iconWidth 32
                , LeafletMap.latitude driver.latitude
                , LeafletMap.longitude driver.longitude
                , LeafletMap.html markerHTML
                , LeafletMap.className "divIconNoBorderNoBackground"
                ]
                [ text driver.name
                ]

        withoutHidden : List Types.DriverLocation -> List Types.DriverLocation
        withoutHidden drivers =
            List.filter (\driver -> not <| List.member driver model.hiddenDrivers) model.drivers
    in
    LeafletMap.view
        [ A.style "width" "100%"
        , A.style "height" "83vh"
        , A.style "position" "relative"
        , A.style "display" "block"
        , LeafletMap.latitude 55.758222
        , LeafletMap.longitude 37.622142
        , LeafletMap.zoom 13
        ]
        (List.map driverMarker <| withoutHidden model.drivers)


viewDriversTable : Model -> Html Msg
viewDriversTable model =
    let
        options =
            []

        head =
            Table.simpleThead
                [ Table.th [] [ text "Отображать?" ]
                , Table.th [] [ text "Имя" ]
                , Table.th [] [ text "Иконка" ]
                ]

        body =
            Table.tbody [] (List.map driverTableRow model.drivers)

        driverTableRow driver =
            Table.tr
                []
                [ Table.td []
                    [ Checkbox.checkbox
                        [ Checkbox.checked (not <| List.member driver.id <| List.map .id model.hiddenDrivers)
                        , Checkbox.onCheck (DriverVisibility driver)
                        ]
                        ""
                    ]
                , Table.td [] [ text driver.name ]
                , Table.td [] [ Utils.viewColoredMarker 25 (unpackCarColor driver.carInfo) ]
                ]
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


unpackCarInfo : Maybe Types.CarInfo -> Types.CarInfo
unpackCarInfo mbCarInfo =
    case mbCarInfo of
        Just carInfo ->
            carInfo

        Nothing ->
            { color = "#ff0000"
            , model = ""
            }


unpackCarColor : Maybe Types.CarInfo -> String
unpackCarColor mbCarInfo =
    (unpackCarInfo mbCarInfo).color
