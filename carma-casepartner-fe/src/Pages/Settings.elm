module Pages.Settings exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Char exposing (isDigit)
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , div
        , h1
        , h2
        , text
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , style
        , target
        )
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Maybe exposing (withDefault)
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Set
import Types exposing (Driver)
import Ui


spinnerSize : String
spinnerSize =
    "10rem"


driverDialogSpinnerSize : String
driverDialogSpinnerSize =
    "2rem"


type alias Flags =
    ()


type alias DriverForm =
    { id : Int
    , name : String
    , phone : String
    , password : String
    , plateNum : String
    , isActive : Bool
    }


type alias Model =
    { addDriverDialogVisibility : Modal.Visibility
    , driverForm : DriverForm
    , driverFormError : String
    , driverSpinnerVisible : Bool
    , drivers :
        { showSpinner : Bool
        , data : List Driver
        , smsDisabled : Set.Set Int
        }
    , editDriverDialogVisibility : Modal.Visibility
    , messageToast : MessageToast Msg
    , navbarState : Navbar.State
    , usermenuState : Dropdown.State
    }


type Msg
    = AddDriverCloseAdd
    | AddDriverCloseCancel
    | CloseDialog
    | DriverAdded (Result Http.Error Int)
    | DriverDeleted (Result Http.Error Int)
    | DriverUpdated (Result Http.Error Int)
    | DriversDownloaded (Result Http.Error (List Driver))
    | EditDriverCloseDelete
    | EditDriverCloseSave
    | EditDriverCloseCancel
    | GoToCases
    | GoToLogout
    | GoToSearchCases
    | InputIsActive Bool
    | InputName String
    | InputPhone String
    | InputPassword String
    | InputPlateNum String
    | NavbarMsg Navbar.State
    | SendSMS Int
    | SendSMSComplete (Result Http.Error Int)
    | ShowAddDriverDialog
    | ShowEditDriverDialog Driver
    | UpdateCustomMessageToast (MessageToast Msg)
    | UsermenuMsg Dropdown.State
    | Settings
    | Instruction


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
    ( { addDriverDialogVisibility = Modal.hidden
      , driverForm =
            { id = 0
            , name = ""
            , phone = ""
            , password = ""
            , plateNum = ""
            , isActive = False
            }
      , driverFormError = ""
      , driverSpinnerVisible = False
      , drivers =
            { showSpinner = True
            , data = []
            , smsDisabled = Set.empty
            }
      , editDriverDialogVisibility = Modal.hidden
      , messageToast =
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
        AddDriverCloseAdd ->
            let
                ( driverFormError, driverSpinnerVisible, cmd ) =
                    case checkDriverForm model of
                        Just e ->
                            ( e
                            , False
                            , Cmd.none
                            )

                        Nothing ->
                            ( ""
                            , True
                            , let
                                driver =
                                    { id = 0
                                    , partner = 0
                                    , name = model.driverForm.name
                                    , phone = model.driverForm.phone
                                    , password = model.driverForm.password
                                    , plateNum = Just model.driverForm.plateNum
                                    , isActive = model.driverForm.isActive
                                    , serviceId = Nothing
                                    }
                              in
                              Api.createDriver driver DriverAdded
                            )
            in
            ( { model
                | driverFormError = driverFormError
                , driverSpinnerVisible = driverSpinnerVisible
              }
            , cmd
            , Cmd.none
            )

        AddDriverCloseCancel ->
            ( { model | addDriverDialogVisibility = Modal.hidden }
            , Cmd.none
            , Cmd.none
            )

        CloseDialog ->
            ( { model
                | addDriverDialogVisibility = Modal.hidden
                , editDriverDialogVisibility = Modal.hidden
              }
            , Cmd.none
            , Cmd.none
            )

        DriverAdded result ->
            case result of
                Err _ ->
                    let
                        newModel =
                            showError "Не могу добавить водителя!"
                    in
                    ( { newModel
                        | driverFormError = "Не могу добавить водителя!"
                        , driverSpinnerVisible = False
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok driverId ->
                    ( { model
                        | addDriverDialogVisibility = Modal.hidden
                      }
                    , Api.getDrivers DriversDownloaded
                    , Cmd.none
                    )

        DriverDeleted _ ->
            -- TODO process error
            ( { model
                | editDriverDialogVisibility = Modal.hidden
              }
            , Api.getDrivers DriversDownloaded
            , Cmd.none
            )

        DriversDownloaded result ->
            case result of
                Err e ->
                    ( showError "Не могу получить список водителей!"
                    , Cmd.none
                    , Cmd.none
                    )

                Ok drivers ->
                    let
                        newDrivers =
                            { showSpinner = False
                            , data = drivers
                            , smsDisabled = Set.empty
                            }
                    in
                    ( { model | drivers = newDrivers }
                    , Cmd.none
                    , Cmd.none
                    )

        DriverUpdated result ->
            case result of
                Err _ ->
                    let
                        newModel =
                            showError "Не могу обновить информацию о водителе!"
                    in
                    ( { newModel
                        | driverFormError = "Не могу обновить информацию о водителе!"
                        , driverSpinnerVisible = False
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok driverId ->
                    ( { model
                        | editDriverDialogVisibility = Modal.hidden
                      }
                    , Api.getDrivers DriversDownloaded
                    , Cmd.none
                    )

        EditDriverCloseDelete ->
            ( { model
                | drivers =
                    { data = []
                    , showSpinner = True
                    , smsDisabled = Set.empty
                    }
              }
            , Api.deleteDriver model.driverForm.id DriverDeleted
            , Cmd.none
            )

        EditDriverCloseCancel ->
            ( { model | editDriverDialogVisibility = Modal.hidden }
            , Cmd.none
            , Cmd.none
            )

        EditDriverCloseSave ->
            let
                ( driverFormError, driverSpinnerVisible, cmd ) =
                    case checkDriverForm model of
                        Just e ->
                            ( e
                            , False
                            , Cmd.none
                            )

                        Nothing ->
                            ( ""
                            , True
                            , let
                                driver =
                                    { id = model.driverForm.id
                                    , partner = 0
                                    , name = model.driverForm.name
                                    , phone = model.driverForm.phone
                                    , password = model.driverForm.password
                                    , plateNum =
                                        if String.isEmpty <| String.trim model.driverForm.plateNum then
                                            Nothing

                                        else
                                            Just model.driverForm.plateNum
                                    , isActive = model.driverForm.isActive
                                    , serviceId = Nothing
                                    }
                              in
                              Api.updateDriver driver DriverUpdated
                            )
            in
            ( { model
                | driverFormError = driverFormError
                , driverSpinnerVisible = driverSpinnerVisible
              }
            , cmd
            , Cmd.none
            )

        InputIsActive checked ->
            let
                newDriverForm =
                    model.driverForm
            in
            ( { model | driverForm = { newDriverForm | isActive = checked } }
            , Cmd.none
            , Cmd.none
            )

        InputName name ->
            let
                newDriverForm =
                    model.driverForm
            in
            ( { model | driverForm = { newDriverForm | name = name } }
            , Cmd.none
            , Cmd.none
            )

        InputPhone phone ->
            let
                newDriverForm =
                    model.driverForm
            in
            ( { model | driverForm = { newDriverForm | phone = phone } }
            , Cmd.none
            , Cmd.none
            )

        InputPassword password ->
            let
                newDriverForm =
                    model.driverForm
            in
            ( { model
                | driverForm =
                    { newDriverForm
                        | password =
                            if String.all Char.isDigit password then
                                password

                            else
                                model.driverForm.password
                    }
              }
            , Cmd.none
            , Cmd.none
            )

        InputPlateNum plateNum ->
            let
                newDriverForm =
                    model.driverForm
            in
            ( { model | driverForm = { newDriverForm | plateNum = plateNum } }
            , Cmd.none
            , Cmd.none
            )

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

        -- Entry point
        NavbarMsg state ->
            ( { model | navbarState = state }
            , Api.getDrivers DriversDownloaded
            , Cmd.none
            )

        SendSMS driverId ->
            let
                { showSpinner, data, smsDisabled } =
                    model.drivers
            in
            ( { model
                | drivers =
                    { showSpinner = showSpinner
                    , data = data
                    , smsDisabled = Set.insert driverId smsDisabled
                    }
              }
            , Api.inviteDriver driverId SendSMSComplete
            , Cmd.none
            )

        SendSMSComplete result ->
            case result of
                Err _ ->
                    let
                        newModel =
                            showError "Не могу отправить СМС водителю!"
                    in
                    ( newModel
                    , Cmd.none
                    , Cmd.none
                    )

                Ok smsId ->
                    ( model
                    , Cmd.none
                    , Cmd.none
                    )

        ShowAddDriverDialog ->
            let
                defaultForm : DriverForm
                defaultForm =
                    { id = 0
                    , name = ""
                    , phone = ""
                    , password = ""
                    , plateNum = ""
                    , isActive = True
                    }
            in
            ( { model
                | addDriverDialogVisibility = Modal.shown
                , driverForm = defaultForm
                , driverSpinnerVisible = False
              }
            , Cmd.none
            , Cmd.none
            )

        ShowEditDriverDialog driver ->
            ( { model
                | editDriverDialogVisibility = Modal.shown
                , driverForm =
                    { id = driver.id
                    , name = driver.name
                    , phone = driver.phone
                    , password = driver.password
                    , plateNum = withDefault "" driver.plateNum
                    , isActive = driver.isActive
                    }
                , driverSpinnerVisible = False
              }
            , Cmd.none
            , Cmd.none
            )

        UpdateCustomMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }
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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Settings"
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
                , ( True, Settings, "Настройки" )
                , ( False, Instruction, "Инструкция" )
                ]
            }
          <|
            div []
                [ h1 [ class "text-center" ] [ text "Настройки" ]
                , div []
                    [ viewDrivers model
                    , model.messageToast
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


viewDrivers : Model -> Html Msg
viewDrivers model =
    let
        hC : Table.CellOption Msg
        hC =
            Table.cellAttr <| class "text-center"

        vC : Table.CellOption Msg
        vC =
            Table.cellAttr <| class "align-middle"
    in
    Grid.row
        [ Row.attrs
            [ Spacing.p1
            , Spacing.m1
            , Flex.row
            , class "border"
            ]
        ]
    <|
        if model.drivers.showSpinner then
            [ Grid.col [ Col.textAlign Text.alignXsCenter ]
                [ Ui.viewSpinner spinnerSize ]
            ]

        else
            [ Grid.col
                [ Col.attrs [ Spacing.p1 ] ]
                [ Grid.row
                    []
                    [ Grid.col
                        [ Col.md11 ]
                        [ h2 [] [ text "Водители" ] ]
                    , Grid.col
                        [ Col.md1, Col.textAlign Text.alignXsRight ]
                        [ Button.button
                            [ Button.primary
                            , Button.attrs [ onClick ShowAddDriverDialog ]
                            ]
                            [ Icon.plus
                                |> Icon.present
                                |> Icon.styled [ Icon.xs ]
                                |> Icon.view
                            ]
                        ]
                    ]
                , viewAddDriverDialog model
                , viewEditDriverDialog model
                , Table.table
                    { options =
                        [ Table.bordered
                        , Table.striped
                        , Table.small
                        , Table.responsiveLg
                        , Table.attr (style "background-color" Ui.colors.casesBg)
                        ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [ hC, vC ] [ text "ФИО" ]
                            , Table.th [ hC, vC ] [ text "Телефон" ]
                            , Table.th [ hC, vC ] [ text "Гос. номер" ]
                            , Table.th [ hC, vC ] [ text "Активный?" ]
                            , Table.th [] []
                            ]
                    , tbody =
                        Table.tbody [] <|
                            List.map (Table.tr []) <|
                                List.map
                                    (\driver ->
                                        [ Table.td [ vC ]
                                            [ Ui.cell driver.name ]
                                        , Table.td [ vC ]
                                            [ Ui.cell driver.phone ]
                                        , Table.td [ hC, vC ]
                                            [ Ui.cell <| withDefault "" driver.plateNum ]
                                        , Table.td [ hC, vC ]
                                            [ Checkbox.checkbox
                                                [ Checkbox.disabled True
                                                , Checkbox.checked driver.isActive
                                                ]
                                                ""
                                            ]
                                        , Table.td
                                            [ Table.cellAttr <| Spacing.pl2
                                            , vC
                                            ]
                                            [ Button.button
                                                [ Button.primary
                                                , Button.attrs
                                                    [ onClick <| ShowEditDriverDialog driver
                                                    , Spacing.ml3
                                                    , Spacing.mr3
                                                    ]
                                                ]
                                                [ Icon.edit
                                                    |> Icon.present
                                                    |> Icon.styled []
                                                    |> Icon.view
                                                ]
                                            , Button.button
                                                [ Button.primary
                                                , Button.disabled <|
                                                    Set.member driver.id
                                                        model.drivers.smsDisabled
                                                , Button.attrs
                                                    [ attribute "data-toggle" "tooltip"
                                                    , attribute "data-placement" "top"
                                                    , attribute "title" "При нажатии на кнопку, Мы отправим смс на номер водителя с ссылкой на мобильное приложение."
                                                    , onClick <| SendSMS driver.id
                                                    , Spacing.ml3
                                                    , Spacing.mr3
                                                    ]
                                                ]
                                                [ Icon.mobileAlt
                                                    |> Icon.present
                                                    |> Icon.styled []
                                                    |> Icon.view
                                                ]
                                            ]
                                        ]
                                    )
                                    model.drivers.data
                    }
                ]
            ]


viewDialog :
    Modal.Visibility
    -> String
    -> List (Html Msg)
    -> List (Html Msg)
    -> Html Msg
viewDialog visibility title body footer =
    Modal.config CloseDialog
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h5 [] [ text title ]
        |> Modal.body [] body
        |> Modal.footer [] footer
        |> Modal.view visibility


driverForm : Model -> List (Html Msg)
driverForm model =
    [ Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
            [ text "ФИО:" ]
        , Grid.col
            []
            [ Input.text
                [ Input.attrs [ onInput InputName ]
                , Input.value model.driverForm.name
                ]
            ]
        ]
    , Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
            [ text "Телефон:" ]
        , Grid.col
            []
            [ Input.text
                [ Input.attrs [ onInput InputPhone ]
                , Input.value model.driverForm.phone
                ]
            ]
        ]
    , Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
            [ text "Пароль:" ]
        , Grid.col
            []
            [ Input.text
                [ Input.attrs [ onInput InputPassword ]
                , Input.value model.driverForm.password
                ]
            ]
        ]
    , Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
            [ text "Гос. номер:" ]
        , Grid.col
            []
            [ Input.text
                [ Input.attrs [ onInput InputPlateNum ]
                , Input.value model.driverForm.plateNum
                ]
            ]
        ]
    , Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
            [ text "Активен? " ]
        , Grid.col
            []
            [ Checkbox.checkbox
                [ Checkbox.checked model.driverForm.isActive
                , Checkbox.attrs [ onCheck InputIsActive ]
                ]
                ""
            ]
        ]
    , Grid.row
        [ Row.attrs [ style "height" driverDialogSpinnerSize ] ]
        [ Grid.col
            [ Col.sm12, Col.textAlign Text.alignXsCenter ]
            [ if model.driverSpinnerVisible then
                Ui.viewSpinner driverDialogSpinnerSize

              else
                text ""
            ]
        ]
    , Grid.row
        [ Row.attrs [ Spacing.pb2 ] ]
        [ Grid.col
            [ Col.sm12 ]
            [ if String.isEmpty model.driverFormError then
                text ""

              else
                Alert.simpleDanger [] [ text model.driverFormError ]
            ]
        ]
    ]


viewAddDriverDialog : Model -> Html Msg
viewAddDriverDialog model =
    viewDialog model.addDriverDialogVisibility
        "Добавить нового водителя:"
        (driverForm model)
        [ Button.button
            [ Button.primary
            , Button.attrs [ onClick AddDriverCloseAdd ]
            , Button.disabled model.driverSpinnerVisible
            ]
            [ text "Добавить" ]
        , Button.button
            [ Button.primary
            , Button.attrs [ onClick AddDriverCloseCancel ]
            ]
            [ text "Отменить" ]
        ]


viewEditDriverDialog : Model -> Html Msg
viewEditDriverDialog model =
    viewDialog model.editDriverDialogVisibility
        "Отредактировать водителя:"
        (driverForm model)
        [ Button.button
            [ Button.danger
            , Button.attrs [ onClick EditDriverCloseDelete, Spacing.mr2 ]
            , Button.disabled model.driverSpinnerVisible
            ]
            [ text "Удалить" ]
        , div [] [ text " " ]
        , Button.button
            [ Button.primary
            , Button.attrs [ onClick EditDriverCloseSave ]
            , Button.disabled model.driverSpinnerVisible
            ]
            [ text "Сохранить" ]
        , Button.button
            [ Button.primary
            , Button.attrs [ onClick EditDriverCloseCancel ]
            ]
            [ text "Отменить" ]
        ]


checkDriverForm : Model -> Maybe String
checkDriverForm model =
    let
        isValidPhone phone =
            (String.length phone == 12)
                && (String.left 2 phone == "+7")
                && String.all Char.isDigit (String.dropLeft 2 phone)

        isValidPassword password =
            (String.length password >= 4)
                && String.all Char.isDigit password
    in
    if String.isEmpty (String.trim model.driverForm.name) then
        Just "Укажите ФИО!"

    else if not <| isValidPhone model.driverForm.phone then
        Just "Номер телефона должен начинаться с '+7' с содержить 10 цифр!"

    else if not <| isValidPassword model.driverForm.password then
        Just "Пароль должен состоять из не менее чем 4 цифр!"

    else
        Nothing
