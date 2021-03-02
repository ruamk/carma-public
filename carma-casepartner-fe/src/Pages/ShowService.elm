module Pages.ShowService exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block exposing (titleH3)
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Chat
import Const
import Dict
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , b
        , br
        , div
        , h2
        , h3
        , hr
        , li
        , p
        , s
        , text
        , ul
        )
import Html.Attributes as A
    exposing
        ( action
        , class
        , placeholder
        , selected
        , style
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import ISO8601
import Maybe exposing (withDefault)
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Time
import Tuple
import Types as Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , CurrentCaseInfo
        , Dictionary
        , Driver
        , Payment
        , ServiceDescription
        , emptyServiceDescription
        , Location
        )
import Ui
import Utils exposing (formatTime)
import Html exposing (a)


type alias Flags =
    ()


type alias Comment =
    { author : String
    , action : String
    , date : String
    , result : String
    , service : String
    }


type ClosingServiceForm
    = RUAMK
        { price : String -- Float
        , red : Bool
        , description : String
        }
    | Client
        { price : String -- Float
        , red : Bool
        }
    | Mixed
        { priceRUAMK : String -- Float
        , ruamkRed : Bool
        , descriptionRUAMK : String
        , priceClient : String -- Float
        , clientRed : Bool
        }
    | Refund
        { price : String -- Float
        , red : Bool
        }


type ClosedBy
    = Operator
        { checkPrice : Float
        , checkTranscipt : String
        , clientPrice : Maybe String
        }
    | Partner
        { partnerPrice : Float
        , partnerTranscipt : String
        , clientPrice : Maybe String
        }


type alias StatusButton1 =
    { disabled : Bool
    , message : Maybe Msg
    }


type alias StatusButton2 =
    { label : String
    , disabled : Bool
    , disabledTime : Int
    , message : Maybe Msg
    }


type alias Model =
    { closing1 : String
    , closing2 : String
    , closing3 : String
    , closing4 : String
    , commentFileName : String
    , comments : List CaseComment
    , commentsDownloaded : Bool
    , drivers : List Driver
    , driverSpinnerVisible : Bool
    , inputComment : String
    , latenessDetails : String
    , latenessDetailsDisabled : Bool
    , latenessHours : String
    , latenessMinutes : String
    , latenessOkDisabled : Bool
    , latenessReason : String
    , latenessReasons : Dictionary
    , latenessVisibility : Modal.Visibility
    , messageToast : MessageToast Msg
    , modalAlertVisibility : Modal.Visibility
    , navbarState : Navbar.State
    , selectedDriver : String -- для Select нужен идентификатор в виде строки
    , assignedDriver : Maybe Driver
    , service : ServiceDescription
    , statusButton1 : StatusButton1
    , statusButton2 : StatusButton2
    , usermenuState : Dropdown.State
    , currentCases : List CurrentCaseInfo
    , typeOfServiceSynonym : Dictionary
    , closingServiceForm : Maybe ClosingServiceForm
    }


type Msg
    = AddComment
    | AddCommentResponse (Result Http.Error Int)
    | ApplySelectDriver
    | CancelDriver
    | Cases
    | Chat String
    | Closing1 String
    | Closing2 String
    | Closing3 String
    | Closing4 String
    | CommentFileName String
    | CommentsDownloaded (Result Http.Error (List CaseComment))
    | DriversDownloaded (Result Http.Error (List Driver))
    | DriverChanged String -- выбор водителя из списка
    | InPlace -- На месте
    | InputComment String
    | LatenessAsk -- Опоздание - запрос параметров
    | LatenessCloseCancel -- нажата Cancel в модальном окне
    | LatenessCloseOk -- нажата Ok в модельном окне
    | LatenessDetailsInput String -- ввод подробностей причины "Другое"
    | LatenessHoursChanged String --
    | LatenessMinutesChanged String --
    | LatenessPostPartnerDelayResponse (Result Http.Error Int)
    | LatenessReasonChanged String -- выбрана причина опоздания
    | LatenessShowModal (Result Http.Error Dictionary) -- показ модального окна Опоздание
    | Logout
    | ModalAlreadyOkClose -- Событие закрытия диалога "Услуга уже оказана"
    | NavbarMsg Navbar.State
    | SearchCases
    | ServiceDownloaded (Result Http.Error ServiceDescription)
    | ServicePerformed -- Услуга оказана
    | ServiceWasAssignedToDriver (Result Http.Error Int)
    | ServiceWasCancelledToDriver (Result Http.Error Int)
    | Settings
    | Tick Time.Posix
    | UpdateComments (Result Http.Error Int) -- Услуга оказана (ответ)
    | UpdateCustomMessageToast (MessageToast Msg)
    | UpdateStatus (Result Http.Error String) -- Результат изменения статуса
    | UsermenuMsg Dropdown.State
    | CurrentCase Int
    | UpdateServicesListTick Time.Posix
    | GetCurrentCases (Result Http.Error (List CurrentCaseInfo))
    | TypeOfServiceSynonymDownloaded (Result Http.Error Dictionary)
    | UpdateClosingServiceForm ClosingServiceForm
    | CloseService
    | ServiceClosed (Result Http.Error Bool)


driverSpinnerSize : String
driverSpinnerSize =
    "2rem"



{- | buttonDisabledTime in seconds -}


buttonDisabledTime : Int
buttonDisabledTime =
    5


commentsUpdateTime : Float
commentsUpdateTime =
    10


servicesListUpdateSeconds : Float
servicesListUpdateSeconds =
    60


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { service = emptyServiceDescription
      , closing1 = ""
      , closing2 = ""
      , closing3 = ""
      , closing4 = ""
      , drivers = []
      , driverSpinnerVisible = True
      , inputComment = ""
      , commentFileName = ""
      , navbarState = navbarState
      , usermenuState = Dropdown.initialState
      , commentsDownloaded = False
      , comments = []
      , statusButton1 =
            { disabled = True
            , message = Nothing
            }
      , statusButton2 =
            { label = ""
            , disabled = True
            , disabledTime = 0
            , message = Nothing
            }
      , modalAlertVisibility = Modal.hidden
      , latenessVisibility = Modal.hidden
      , latenessHours = "0"
      , latenessMinutes = "0"
      , latenessReason = ""
      , latenessReasons = Dict.empty
      , latenessDetails = ""
      , latenessDetailsDisabled = True
      , latenessOkDisabled = True
      , messageToast =
            MessageToast.initWithConfig UpdateCustomMessageToast
                { delayInMs = 2000
                , toastsToShow = 10
                }
      , selectedDriver = ""
      , assignedDriver = Nothing
      , currentCases = []
      , typeOfServiceSynonym = Dict.empty
      , closingServiceForm = Nothing
      }
    , navbarCmd
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    let
        checkOkDisabled : Model -> Bool
        checkOkDisabled m =
            let
                other : String
                other =
                    String.fromInt Const.latenessReasons.other

                details : String
                details =
                    String.trim m.latenessDetails
            in
            if m.latenessHours ++ ":" ++ m.latenessMinutes == "0:0" then
                {- If time is not specified -}
                True

            else if m.latenessReason == "" then
                {- If reason not specified -}
                True

            else if m.latenessReason == other && details == "" then
                {- If reason is "Other" and details are not specified -}
                True

            else
                False
    in
    case msg of
        -- Entry point
        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.batch
                [ Api.getService global.serviceId ServiceDownloaded
                , Api.getDrivers DriversDownloaded
                , Api.getTypeOfServiceSynonym TypeOfServiceSynonymDownloaded
                , Api.getLatestCurrentCases GetCurrentCases
                ]
            , Cmd.none
            )

        ServiceDownloaded result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage
                                    ("Error get case in showcase "
                                        ++ String.fromInt global.serviceId
                                    )
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok service ->
                    let
                        statusButton1 : StatusButton1
                        statusButton1 =
                            if service.status == Const.serviceStatus.ordered then
                                { disabled = False
                                , message = Just LatenessAsk
                                }

                            else
                                { disabled = True
                                , message = Nothing
                                }

                        statusButton2 : StatusButton2
                        statusButton2 =
                            if service.status == Const.serviceStatus.ordered then
                                { label = "На месте"
                                , disabled = False
                                , disabledTime = 0
                                , message = Just InPlace
                                }

                            else if service.status == Const.serviceStatus.inProgress then
                                { label = "Услуга выполнена"
                                , disabled = False
                                , disabledTime = 0
                                , message = Just ServicePerformed
                                }

                            else
                                { label = "Услуга оказана"
                                , disabled = True
                                , disabledTime = 0
                                , message = Nothing
                                }
                    in
                    ( { model
                        | service = service
                        , closingServiceForm =
                            let
                                makeEmpty form =
                                    case form of
                                        RUAMK f ->
                                            RUAMK
                                                { price = ""
                                                , description = ""
                                                , red = False
                                                }

                                        Client f ->
                                            Client
                                                { price = ""
                                                , red = False
                                                }

                                        Mixed f ->
                                            Mixed
                                                { priceRUAMK = ""
                                                , ruamkRed = False
                                                , descriptionRUAMK = ""
                                                , priceClient = ""
                                                , clientRed = False
                                                }

                                        Refund f ->
                                            Refund
                                                { price = ""
                                                , red = False
                                                }

                                emptyForm n =
                                    case service.payType of
                                        Just 1 ->
                                            Just <|
                                                RUAMK
                                                    { price = ""
                                                    , description = ""
                                                    , red = False
                                                    }

                                        Just 2 ->
                                            Just <|
                                                Client
                                                    { price = ""
                                                    , red = False
                                                    }

                                        Just 3 ->
                                            Just <|
                                                Mixed
                                                    { priceRUAMK = ""
                                                    , ruamkRed = False
                                                    , descriptionRUAMK = ""
                                                    , priceClient = ""
                                                    , clientRed = False
                                                    }

                                        Just 4 ->
                                            Just <|
                                                Refund
                                                    { price = ""
                                                    , red = False
                                                    }

                                        _ ->
                                            Nothing
                            in
                            case model.closingServiceForm of
                                Just f ->
                                    case Maybe.map2 (==) (Just <| makeEmpty f) (emptyForm (Maybe.withDefault 0 service.payType)) of
                                        Just True ->
                                            Just f

                                        Just False ->
                                            emptyForm (Maybe.withDefault 0 service.payType)

                                        Nothing ->
                                            Nothing

                                Nothing ->
                                    emptyForm (Maybe.withDefault 0 service.payType)
                        , statusButton1 = statusButton1
                        , statusButton2 = statusButton2
                      }
                    , Api.getServiceComments global.serviceId CommentsDownloaded
                    , Cmd.none
                    )

        ApplySelectDriver ->
            let
                cmd =
                    case String.toInt model.selectedDriver of
                        Nothing ->
                            Cmd.none

                        Just driverId ->
                            Api.assignServiceToDriver
                                global.serviceId
                                driverId
                                ServiceWasAssignedToDriver
            in
            ( model
            , cmd
            , Cmd.none
            )

        CancelDriver ->
            let
                cmd =
                    case model.assignedDriver of
                        Nothing ->
                            Cmd.none

                        Just driver ->
                            Api.cancelServiceToDriver
                                global.serviceId
                                driver.id
                                ServiceWasCancelledToDriver
            in
            ( { model | driverSpinnerVisible = True }
            , cmd
            , Cmd.none
            )

        ServiceWasAssignedToDriver result ->
            ( case result of
                Err _ ->
                    { model
                        | messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Не могу назначить услугу на водителя!"
                    }

                Ok _ ->
                    model
            , -- обновление списка водителей для получения новых serviceId
              Api.getDrivers DriversDownloaded
            , Cmd.none
            )

        ServiceWasCancelledToDriver result ->
            ( case result of
                Err _ ->
                    { model
                        | messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Не могу отменить услугу для водителя!"
                    }

                Ok _ ->
                    model
            , -- обновление списка водителей для получения новых serviceId
              Api.getDrivers DriversDownloaded
            , Cmd.none
            )

        DriverChanged driverId ->
            ( { model | selectedDriver = driverId }
            , Cmd.none
            , Cmd.none
            )

        DriversDownloaded result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Error get drivers"
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok drivers ->
                    ( { model
                        | drivers = drivers
                        , driverSpinnerVisible = False
                        , assignedDriver =
                            let
                                ds =
                                    List.filter
                                        (\d -> withDefault -1 d.serviceId == global.serviceId)
                                        drivers
                            in
                            case ds of
                                d :: _ ->
                                    Just d

                                _ ->
                                    Nothing
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        CommentsDownloaded result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "error get comments in showcase "
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok comments ->
                    ( { model
                        | comments = comments
                        , commentsDownloaded = True
                      }
                    , Chat.connectToCase global.url model.service.caseId
                    , Cmd.none
                    )

        Cases ->
            ( model
            , Cmd.none
            , Global.navigate Route.Services
            )

        SearchCases ->
            ( model
            , Cmd.none
            , Global.navigate Route.Search
            )

        Closing1 s ->
            ( { model | closing1 = s }
            , Cmd.none
            , Cmd.none
            )

        Closing2 s ->
            ( { model | closing2 = s }
            , Cmd.none
            , Cmd.none
            )

        Closing3 s ->
            ( { model | closing3 = s }
            , Cmd.none
            , Cmd.none
            )

        Closing4 s ->
            ( { model | closing4 = s }
            , Cmd.none
            , Cmd.none
            )

        InputComment s ->
            ( { model | inputComment = s }
            , Cmd.none
            , Cmd.none
            )

        CommentFileName s ->
            ( { model | commentFileName = s }
            , Cmd.none
            , Cmd.none
            )

        AddComment ->
            ( { model | inputComment = "" }
            , Api.postServiceComment model.service.caseId
                { comment = model.inputComment }
                AddCommentResponse
            , Cmd.none
            )

        AddCommentResponse result ->
            ( model
            , Api.getServiceComments global.serviceId CommentsDownloaded
            , Cmd.none
            )

        UsermenuMsg state ->
            ( { model | usermenuState = state }
            , Cmd.none
            , Cmd.none
            )

        InPlace ->
            ( model
            , Api.statusInPlace global.serviceId UpdateStatus
            , Cmd.none
            )

        LatenessAsk ->
            ( model
            , Api.getLatenessReasons LatenessShowModal
            , Cmd.none
            )

        LatenessShowModal result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "unable to get lateness reasons"
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok reasons ->
                    ( { model
                        | latenessReasons = reasons
                        , latenessOkDisabled = checkOkDisabled model
                        , latenessVisibility = Modal.shown
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        LatenessCloseCancel ->
            ( { model | latenessVisibility = Modal.hidden }
            , Cmd.none
            , Cmd.none
            )

        LatenessCloseOk ->
            let
                minutes : Int
                minutes =
                    (Maybe.withDefault 0 <| String.toInt model.latenessHours)
                        * 60
                        + (Maybe.withDefault 0 <| String.toInt model.latenessMinutes)

                comment : Maybe String
                comment =
                    if model.latenessReason == String.fromInt Const.latenessReasons.other then
                        Just model.latenessDetails

                    else
                        Nothing
            in
            ( { model
                | latenessDetails = ""
                , latenessReason = ""
                , latenessVisibility = Modal.hidden
              }
            , Api.postPartnerDelay global.serviceId
                { minutes = minutes
                , reason = Maybe.withDefault -1 <| String.toInt model.latenessReason
                , comment = comment
                }
                LatenessPostPartnerDelayResponse
            , Cmd.none
            )

        LatenessPostPartnerDelayResponse result ->
            case result of
                Err _ ->
                    let
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Error create partner delay."
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok _ ->
                    ( model
                    , Api.getServiceComments global.serviceId CommentsDownloaded
                    , Cmd.none
                    )

        LatenessReasonChanged v ->
            let
                latenessDetailsDisabled : Bool
                latenessDetailsDisabled =
                    v /= String.fromInt Const.latenessReasons.other

                model_ : Model
                model_ =
                    { model | latenessReason = v }

                latenessOkDisabled : Bool
                latenessOkDisabled =
                    checkOkDisabled model_
            in
            ( { model_
                | latenessOkDisabled = latenessOkDisabled
                , latenessDetailsDisabled = latenessDetailsDisabled
              }
            , Cmd.none
            , Cmd.none
            )

        LatenessHoursChanged v ->
            let
                model_ : Model
                model_ =
                    { model | latenessHours = v }

                latenessOkDisabled : Bool
                latenessOkDisabled =
                    checkOkDisabled model_
            in
            ( { model_
                | latenessOkDisabled = latenessOkDisabled
              }
            , Cmd.none
            , Cmd.none
            )

        LatenessMinutesChanged v ->
            let
                model_ : Model
                model_ =
                    { model | latenessMinutes = v }

                latenessOkDisabled : Bool
                latenessOkDisabled =
                    checkOkDisabled model_
            in
            ( { model_
                | latenessOkDisabled = latenessOkDisabled
              }
            , Cmd.none
            , Cmd.none
            )

        LatenessDetailsInput s ->
            let
                model_ : Model
                model_ =
                    { model | latenessDetails = s }

                latenessOkDisabled : Bool
                latenessOkDisabled =
                    checkOkDisabled model_
            in
            ( { model_
                | latenessOkDisabled = latenessOkDisabled
              }
            , Cmd.none
            , Cmd.none
            )

        ServicePerformed ->
            ( model
            , Api.statusServicePerformed global.serviceId
                ("Партнёр выполнил услугу " ++ formatServiceSerial model)
                UpdateStatus
            , Cmd.none
            )

        UpdateComments result ->
            ( model
            , Api.getServiceComments global.serviceId CommentsDownloaded
            , Cmd.none
            )

        Tick _ ->
            let
                s : StatusButton2
                s =
                    model.statusButton2
            in
            ( { model
                | statusButton2 =
                    if model.statusButton2.disabledTime > 0 then
                        { s
                            | disabledTime = model.statusButton2.disabledTime - 1
                            , disabled = True
                        }

                    else if model.service.status == Const.serviceStatus.ok then
                        { s | disabled = True }

                    else
                        { s | disabled = False }
              }
            , if model.statusButton2.disabledTime == 0 && model.statusButton2.disabled then
                Api.getService global.serviceId ServiceDownloaded

              else
                Cmd.none
            , Cmd.none
            )

        UpdateStatus status ->
            case status of
                Ok "service_performed" ->
                    ( { model
                        | statusButton2 =
                            { label = "Услуга оказана"
                            , disabled = True
                            , disabledTime = buttonDisabledTime
                            , message = Nothing
                            }
                      }
                    , Api.getServiceComments global.serviceId CommentsDownloaded
                    , Cmd.none
                    )

                Ok "service_already_performed" ->
                    ( { model
                        | statusButton2 =
                            { label = "Услуга оказана"
                            , disabled = True
                            , disabledTime = buttonDisabledTime
                            , message = Nothing
                            }
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok unknown ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.warning
                                |> MessageToast.withMessage
                                    ("UpdateStatus unknown response: "
                                        ++ unknown
                                    )
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "UpdateStatus error "
                    in
                    ( { model | messageToast = messageToast }
                    , Cmd.none
                    , Cmd.none
                    )

        Logout ->
            ( model
            , Cmd.none
            , Global.logout
            )

        Settings ->
            ( model
            , Cmd.none
            , Global.settings
            )

        ModalAlreadyOkClose ->
            ( { model | modalAlertVisibility = Modal.hidden }
            , Cmd.none
            , Cmd.none
            )

        UpdateCustomMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }
            , Cmd.none
            , Cmd.none
            )

        Chat message ->
            {-
               let
                   formatUser { ip, id } =
                       String.fromInt id ++ ", " ++ ip

                   messageToast =
                       case Chat.decodeChat message of
                           Ok (Chat.Joined user) ->
                               model.messageToast
                                   |> MessageToast.danger
                                   |> MessageToast.withMessage ("Оператор " ++ formatUser user ++ " вошёл в кейс")

                           Ok (Chat.Left user) ->
                               model.messageToast
                                   |> MessageToast.success
                                   |> MessageToast.withMessage ("Оператор " ++ formatUser user ++ " вышел из кейса")

                           Ok (Chat.YouAreNotAlone users) ->
                               List.foldl
                                   (\user toast ->
                                       toast
                                           |> MessageToast.danger
                                           |> MessageToast.withMessage ("Оператор " ++ formatUser user ++ " работает с кейсом")
                                   )
                                   model.messageToast
                                   users

                           Ok (Chat.Message chatMessage user) ->
                               model.messageToast
                                   |> MessageToast.warning
                                   |> MessageToast.withMessage ("Оператор " ++ formatUser user ++ " оставил сообщение: " ++ chatMessage)

                           Err e ->
                               model.messageToast
                                   |> MessageToast.danger
                                   |> MessageToast.withMessage ("Неизвестное сообщение из чата: " ++ e)
               in
               ( { model | messageToast = messageToast }
            -}
            ( model
            , Cmd.none
            , Cmd.none
            )

        UpdateServicesListTick _ ->
            ( { model
                | currentCases = []
              }
            , Api.getLatestCurrentCases GetCurrentCases
            , Cmd.none
            )

        GetCurrentCases result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Error get current latest cases "
                    in
                    ( { model
                        | messageToast = messageToast
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok currentCases ->
                    ( { model
                        | currentCases = currentCases
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        CurrentCase serviceId ->
            ( model
            , Cmd.none
            , Cmd.batch
                [ Global.serviceId serviceId
                , Global.navigate Route.ShowService
                ]
            )

        TypeOfServiceSynonymDownloaded result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    , Cmd.none
                    )

                Ok typeOfServiceSynonym ->
                    ( { model
                        | typeOfServiceSynonym = typeOfServiceSynonym
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        UpdateClosingServiceForm new ->
            ( { model | closingServiceForm = Just new }
            , Cmd.none
            , Cmd.none
            )

        CloseService ->
            let
                extractForm : ClosingServiceForm -> Maybe ( Float, String, Float )
                extractForm csf =
                    let
                        extract =
                            Maybe.map3 (\partner transcript client -> ( partner, transcript, client ))
                    in
                    case csf of
                        RUAMK p ->
                            extract (String.toFloat p.price) (Just p.description) (Just 0)

                        Client p ->
                            extract (Just 0) (Just "") (String.toFloat p.price)

                        Mixed p ->
                            extract (String.toFloat p.priceRUAMK) (Just "") (String.toFloat p.priceClient)

                        Refund p ->
                            extract (Just 0) (Just "") (String.toFloat p.price)
            in
            case model.closingServiceForm |> Maybe.andThen extractForm of
                Just ( partner, partnerTranscript, client ) ->
                    ( model
                    , Api.closeService global.serviceId partner partnerTranscript client ServiceClosed
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "You cant send an undefined form"
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        ServiceClosed response ->
            case response of
                Err e ->
                    ( { model
                        | messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Ошибка при закрытии услуги."
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok status ->
                    case status of
                        True ->
                            ( { model
                                | messageToast =
                                    model.messageToast
                                        |> MessageToast.success
                                        |> MessageToast.withMessage "Услуга отправлена на закрытие."
                              }
                            , Cmd.none
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | messageToast =
                                    model.messageToast
                                        |> MessageToast.danger
                                        |> MessageToast.withMessage "произошло что то очень плохое...."
                              }
                            , Cmd.none
                            , Cmd.none
                            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Dropdown.subscriptions model.usermenuState UsermenuMsg
        , Chat.caseReceiver Chat
        , Time.every (commentsUpdateTime * 1000) Tick
        , Time.every (servicesListUpdateSeconds * 1000) UpdateServicesListTick
        ]


formatServiceSerial : Model -> String
formatServiceSerial model =
    let
        s : ServiceDescription
        s =
            model.service
    in
    String.fromInt s.caseId
        ++ (if s.services > 1 then
                "/" ++ String.fromInt s.services

            else
                ""
           )


view : Global.Model -> Model -> Document Msg
view global model =
    { title = formatServiceSerial model
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
                [ ( False, Cases, "Текущие заявки" )
                , ( False, SearchCases, "Поиск заявок" )
                , ( False, Settings, "Настройки" )
                ]
            }
          <|
            div []
                [ viewCasePanel model global.serviceId
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


viewModalAlreadyOk : Model -> Html Msg
viewModalAlreadyOk model =
    Modal.config ModalAlreadyOkClose
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Внимание" ]
        |> Modal.body [] [ p [] [ text "Услуга уже оказана" ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick ModalAlreadyOkClose ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view model.modalAlertVisibility


viewModalLatenessAsk : Model -> Html Msg
viewModalLatenessAsk model =
    let
        sItem : Int -> Select.Item Msg
        sItem i =
            let
                t : String
                t =
                    String.fromInt i
            in
            Select.item [ value t ] [ text t ]

        hours : List (Select.Item Msg)
        hours =
            List.map sItem <| List.range 0 23

        minutes : List (Select.Item Msg)
        minutes =
            List.map sItem <| List.map ((*) 5) <| List.range 0 11

        {- сортировка нужна чтобы варианты были по алфавиту,
           а пустой элемент вначале вынуждает пользователя выбирать
        -}
        reasons : List (Select.Item msg)
        reasons =
            ( "", "" )
                :: Dict.toList model.latenessReasons
                |> List.sortBy Tuple.second
                |> List.map (\( v, k ) -> Select.item [ value v ] [ text k ])
    in
    Modal.config LatenessCloseCancel
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Укажите параметры опоздания." ]
        |> Modal.body []
            [ Grid.row [ Row.attrs [ Spacing.pb2 ] ]
                [ Grid.col
                    [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
                    [ text "Время опоздания:" ]
                , Grid.col []
                    [ div [ class "d-flex flex-row" ]
                        [ div []
                            [ Select.select
                                [ Select.attrs [ style "width" "60px" ]
                                , Select.onChange LatenessHoursChanged
                                ]
                                hours
                            ]
                        , div [ Flex.alignSelfCenter, Spacing.pl1, Spacing.pr3 ]
                            [ text "ч." ]
                        , div []
                            [ Select.select
                                [ Select.attrs [ style "width" "60px" ]
                                , Select.onChange LatenessMinutesChanged
                                ]
                                minutes
                            ]
                        , div [ Flex.alignSelfCenter, Spacing.pl1 ]
                            [ text "мин." ]
                        ]
                    ]
                ]
            , Grid.row [ Row.attrs [ Spacing.pb2 ] ]
                [ Grid.col
                    [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
                    [ text "Причина опоздания:" ]
                , Grid.col []
                    [ Select.select
                        [ Select.onChange LatenessReasonChanged ]
                        reasons
                    ]
                ]
            , Grid.row [ Row.attrs [ Spacing.pb2 ] ]
                [ Grid.col
                    [ Col.sm4, Col.attrs [ Flex.alignSelfCenter ] ]
                    [ text "Подробности:" ]
                , Grid.col []
                    [ Textarea.textarea
                        ((if model.latenessDetailsDisabled then
                            [ Textarea.disabled ]

                          else
                            []
                         )
                            ++ [ Textarea.rows 3
                               , Textarea.value model.latenessDetails
                               , Textarea.attrs [ onInput LatenessDetailsInput ]
                               ]
                        )
                    ]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.disabled model.latenessOkDisabled
                , Button.attrs [ onClick LatenessCloseOk ]
                ]
                [ text "Подтвердить" ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick LatenessCloseCancel ]
                ]
                [ text "Отменить" ]
            ]
        |> Modal.view model.latenessVisibility


asUl : Maybe (Dict.Dict String Bool) -> Html Msg
asUl l =
    case l of
        Nothing ->
            div [] []

        Just d ->
            Dict.filter (\_ v -> v) d
                |> Dict.toList
                |> List.map (\( k, _ ) -> li [] [ text k ])
                |> ul [ style "padding-inline-start" "0px" ]


viewCasePanel : Model -> Int -> Html Msg
viewCasePanel model serviceId =
    let
        c : ServiceDescription
        c =
            model.service

        name : String -> Html Msg
        name t =
            div [ class "name" ] [ text t ]

        value : Html Msg -> Html Msg
        value t =
            div [ class "value" ] [ t ]

        field : String -> Html Msg -> Html Msg
        field n v =
            Grid.row [ Row.attrs [ Spacing.p1 ] ]
                [ Grid.col [ Col.sm5 ]
                    [ name <| n ++ ": " ]
                , Grid.col [ Col.sm7 ]
                    [ value v ]
                ]

        optionalField : String -> String -> Html Msg
        optionalField n v =
            if String.isEmpty v then
                br [] []

            else
                field n <| text v

        caseId : String
        caseId =
            formatServiceSerial model

        vin : String
        vin =
            case c.vin of
                Just v ->
                    v

                Nothing ->
                    "отсутствует"

        formatTime_ : Maybe ISO8601.Time -> String
        formatTime_ t =
            case t of
                Just tt ->
                    let
                        ( date, time ) =
                            formatTime tt
                    in
                    date ++ " " ++ time

                Nothing ->
                    ""

        driverField : Html Msg
        driverField =
            if model.service.status /= Const.serviceStatus.ok then
                field "Назначить водителя" <|
                    if model.driverSpinnerVisible then
                        Ui.viewSpinner driverSpinnerSize

                    else
                        case model.assignedDriver of
                            Nothing ->
                                div []
                                    [ Select.select
                                        [ Select.attrs [ Spacing.mb2 ]
                                        , Select.onChange DriverChanged
                                        ]
                                      <|
                                        Select.item
                                            [ A.value "" ]
                                            [ text "Выберите водителя" ]
                                            :: List.map
                                                (\d ->
                                                    Select.item
                                                        [ A.value <| String.fromInt d.id
                                                        ]
                                                        [ text <|
                                                            d.name
                                                                ++ " ("
                                                                ++ d.phone
                                                                ++ ") "
                                                                ++ withDefault "" d.plateNum
                                                        ]
                                                )
                                                (List.filter (\d -> d.serviceId == Nothing)
                                                    model.drivers
                                                )
                                    , br [] []
                                    , Button.button
                                        [ Button.primary
                                        , Button.attrs [ onClick ApplySelectDriver ]
                                        ]
                                        [ text "Назначить" ]
                                    ]

                            Just driver ->
                                div []
                                    [ text <|
                                        driver.name
                                            ++ " ("
                                            ++ driver.phone
                                            ++ ") "
                                            ++ withDefault "" driver.plateNum
                                    , br [] []
                                    , Button.button
                                        [ Button.primary
                                        , Button.attrs [ Spacing.mt3, onClick CancelDriver ]
                                        ]
                                        [ text "Отменить водителя" ]
                                    ]

            else
                div [] []
        
        locationLink : Location -> Maybe String
        locationLink location = 
            let 
                locationLink_ latitude longitude = 
                    "https://maps.yandex.ru/?z=18&l=map&pt="
                        ++ String.fromFloat longitude
                        ++ ","
                        ++ String.fromFloat latitude
            in 
                Maybe.map2 
                    locationLink_ 
                    location.latitude 
                    location.longitude

        
        viewAddress : Maybe Location -> String -> Html a
        viewAddress mbLocation addressText =
            case mbLocation of 
                Just location -> 
                    case locationLink location of  
                        Just link -> 
                            a [ A.href link, A.target "_blank" ] [ text addressText ]
                        
                        Nothing -> 
                            text addressText 

                Nothing -> 
                    text addressText
        
        formatPaymentType n =
            case n of
                1 ->
                    "РАМК"
                2 ->
                    "Клиент"
                3 ->
                    "Смешанный"
                4 ->
                    "Клиент с возмещением"
                _ ->
                    "неизвестен"

    in
    Grid.row [ Row.attrs [ Spacing.p1 ] ]
        (if model.service.caseId == 0 then
            [ Grid.col [ Col.textAlign Text.alignXsCenter ]
                [ Ui.viewSpinner "10rem"
                ]
            ]

         else
            [ Grid.col [ Col.sm2 ]
                [ viewServicesList
                    model
                    model.currentCases
                ] -- Заявка: 666666 | Оплата: РАМК
            , Grid.col [ Col.attrs [ style "background-color" Ui.colors.casesBg ], Col.sm7 ]
                [ h2 [ class "text-center" ] 
                    [ text <| 
                        "Заявка: " 
                            ++ caseId 
                            ++ " | Оплата: " 
                            ++ formatPaymentType (Maybe.withDefault 0 c.payType)
                    ]
                , Grid.row []
                    [ Grid.col [] 
                        [ field "Вид помощи" <| text c.serviceType
                        , field "Клиент" <| text c.client
                        , field "Телефон клиента" <| a [A.href ("tel:" ++ c.clientPhone)] [ text c.clientPhone ]
                        ]
                    , Grid.col []
                        [ field "Марка/Модель" <| text c.makeModel
                        , field "Гос. номер" <| text c.plateNumber
                        , field "VIN" <| text <| String.toUpper vin
                        ]
                    , Grid.colBreak []
                    , Grid.col []
                        [ field "Адрес начала работы" <| viewAddress c.firstLocation c.firstAddress
                        , field "Желаемая дата оказания услуг" <| text (formatTime_ c.expectedServiceStart)
                        , field "Факт. время оказания услуг" <| text (formatTime_ c.factServiceStart)
                        , field "Время окончания работы" <| text (formatTime_ c.factServiceEnd)
                        ]
                    , Grid.col []
                        [ if c.serviceType == "Техпомощь" then
                            div [] []

                          else
                            optionalField "Адрес доставки" <| c.lastAddress
                        ]
                    ]
                , hr [] []
                , Grid.row []
                    [ Grid.col []
                        [ field "Текущий статус заявки" <|
                            text <|
                                c.statusLabel
                                    ++ " "
                                    ++ "("
                                    ++ String.fromInt c.status
                                    ++ ")"
                        , driverField
                        ]
                    , Grid.col [ Col.attrs [ class "text-center" ] ]
                        [ div [] [ text "Изменить статус" ]
                        , let
                            width : Html.Attribute Msg
                            width =
                                style "width" "150px"
                          in
                          div []
                            [ Button.button
                                [ Button.primary
                                , Button.disabled model.statusButton1.disabled
                                , Button.attrs
                                    (Spacing.m3
                                        :: width
                                        :: (case model.statusButton1.message of
                                                Just m ->
                                                    [ onClick m ]

                                                _ ->
                                                    []
                                           )
                                    )
                                ]
                                [ text "Опоздание" ]
                            , viewModalLatenessAsk model
                            , Button.button
                                [ Button.primary
                                , Button.disabled model.statusButton2.disabled
                                , Button.attrs
                                    (Spacing.m3
                                        :: width
                                        :: (case model.statusButton2.message of
                                                Just m ->
                                                    [ onClick m ]

                                                _ ->
                                                    []
                                           )
                                    )
                                ]
                                [ text <|
                                    model.statusButton2.label
                                        ++ (if model.statusButton2.disabledTime > 0 then
                                                " (" ++ String.fromInt model.statusButton2.disabledTime ++ ")"

                                            else
                                                ""
                                           )
                                ]
                            , viewModalAlreadyOk model
                            ]
                        ]
                    ]
                , hr [] []
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.p3 ] ]
                        [ Form.form [ action "#", onSubmit AddComment ]
                            [ Form.group []
                                [ Textarea.textarea
                                    [ Textarea.id "comment"
                                    , Textarea.rows 3
                                    , Textarea.value model.inputComment
                                    , Textarea.attrs
                                        [ placeholder "Добавить комментарий"
                                        , onInput InputComment
                                        ]
                                    ]
                                ]
                            , Form.row []
                                [ {- Form.col [ Col.sm4, Col.attrs [ Spacing.pt2 ] ]
                                         [ InputGroup.config
                                             (InputGroup.text [ Input.attrs [ placeholder "Добавить файл" ] ])
                                             |> InputGroup.successors
                                                 [ InputGroup.button [ Button.secondary ]
                                                     [ Icon.folderOpen
                                                         |> Icon.present
                                                         |> Icon.styled [ Icon.xs ]
                                                         |> Icon.view
                                                     ]
                                                 ]
                                             |> InputGroup.view
                                         ]
                                     ,
                                  -}
                                  Form.col [ Col.sm4, Col.attrs [ Spacing.pt2 ] ] []
                                , Form.col
                                    [ Col.sm4
                                    , Col.attrs [ Flex.alignItemsEnd, Spacing.pt2 ]
                                    ]
                                    [ Button.button
                                        [ Button.success
                                        , Button.disabled (String.isEmpty model.inputComment)
                                        , Button.attrs
                                            [ class "float-right"
                                            ]
                                        ]
                                        [ text "Добавить комментарий!" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , viewLog model
                ]
            , Grid.col [ Col.sm3, Col.attrs [ Spacing.m0 ] ]
                [ h2 [ class "text-center" ] [ text "Закрыть заявку:" ]
                , Alert.simpleSuccess []
                    [ text "Закрытие заявки не гарантирует оплату в размере Закрытия. "
                    , text "Выставленные счета будут проверяться отделом по обработке "
                    , text "первичной документации на соответствие условиям договора"
                    ]
                , Alert.simpleDanger []
                    [ text "Счета на обработку и оплату принимаются в сканированном виде, "
                    , text "проверяются ООС на соответствие условий в договоре и правил "
                    , text "бухгалтерского учета"
                    ]
                , div [ Spacing.ml0 ]
                    [ if model.service.status == Const.serviceStatus.closed then
                        let
                            convertPayType n =
                                case n of
                                    1 ->
                                        Just Types.RUAMK

                                    2 ->
                                        Just Types.Client

                                    3 ->
                                        Just Types.Mixed

                                    4 ->
                                        Just Types.Refund

                                    _ ->
                                        Nothing

                            needed =
                                Maybe.map2
                                    (\a b -> ( a, b ))
                                    model.service.payment
                                    (model.service.payType |> Maybe.andThen convertPayType)
                        in
                        case needed of
                            Just ( payment_, paytype ) ->
                                viewClosingCaseForClosed payment_ paytype

                            Nothing ->
                                div [] [ text "couldnt fetch payment" ]

                      else
                        viewClosingCaseField model
                    ]
                ]
            ]
        )


viewDetails : CaseComment -> List (Html Msg)
viewDetails { details } =
    let
        field : String -> String -> List (Html Msg)
        field n v =
            [ b [] [ text <| n ++ ": " ]
            , text v
            ]

        fieldOrHide : String -> Maybe String -> List (Html Msg)
        fieldOrHide n v =
            case v of
                Just v_ ->
                    field n v_

                Nothing ->
                    []

        rows : List (List (Html Msg))
        rows =
            case details of
                Just (Types.CaseCommentAction { type_, result, comment, serviceLabel }) ->
                    [ field "Действие" type_
                    , field "Результат" result
                    , case serviceLabel of
                        Just serviceLabel_ ->
                            field "Услуга" serviceLabel_

                        Nothing ->
                            []
                    , case comment of
                        Just comment_ ->
                            field "Комментарий" comment_

                        Nothing ->
                            []
                    ]

                Just (CaseCommentAvayaEvent { aeType, aeCall, aeInterLocutors }) ->
                    [ field "Событие AVAYA" aeType
                    , if List.isEmpty aeInterLocutors then
                        []

                      else
                        field "Второй абонент" <| String.join " " aeInterLocutors
                    , field "Идентификатор звонка" aeCall
                    ]

                Just (CaseCommentCall { callType }) ->
                    [ field "Звонок" callType ]

                Just (CaseCommentComment { commentText }) ->
                    [ field "Комментарий" commentText ]

                Just (CaseCommentPartnerCancel _) ->
                    [ field "Отказ партнёра" "" ]

                Just (CaseCommentPartnerDelay params) ->
                    let
                        { serviceLabel, delayMinutes, delayConfirmed } =
                            params

                        time : Maybe String
                        time =
                            case delayMinutes of
                                Just mm ->
                                    let
                                        hours : Int
                                        hours =
                                            mm // 60

                                        minutes : Int
                                        minutes =
                                            mm - hours * 60
                                    in
                                    Just <|
                                        String.padLeft 2 '0' (String.fromInt hours)
                                            ++ ":"
                                            ++ String.padLeft 2 '0' (String.fromInt minutes)

                                Nothing ->
                                    Nothing
                    in
                    [ fieldOrHide "Услуга" serviceLabel
                    , fieldOrHide "Время опоздания" time
                    , fieldOrHide "Опоздание согласовано" delayConfirmed
                    ]

                Just (CaseCommentSmsForPartner { msgText, phone, deliveryStatus }) ->
                    [ field "Партнёру отправлено SMS" msgText
                    , field "Телефон получателя" phone
                    , field "Статус отправки" deliveryStatus
                    ]

                _ ->
                    [ field "Неопределено" ""
                    ]
    in
    List.map (\cols -> Grid.row [] [ Grid.col [] cols ]) rows


viewLog : Model -> Html Msg
viewLog model =
    if model.commentsDownloaded then
        div [] <|
            List.map
                (\l ->
                    let
                        dt : String
                        dt =
                            case l.datetime of
                                Just dt_ ->
                                    let
                                        ( d, t ) =
                                            formatTime dt_
                                    in
                                    d ++ " " ++ t

                                Nothing ->
                                    ""
                    in
                    Alert.simpleLight [] <|
                        Grid.row []
                            [ Grid.col [ Col.sm4 ]
                                [ text dt ]
                            , Grid.col
                                [ Col.sm8
                                , Col.attrs [ Flex.alignItemsEnd ]
                                ]
                                [ div [ class "float-right" ]
                                    [ text <| Maybe.withDefault "" l.who ]
                                ]
                            ]
                            :: viewDetails l
                )
                model.comments

    else
        Grid.row []
            [ Grid.col [ Col.textAlign Text.alignXsCenter ] <|
                [ Ui.viewSpinner "10rem" ]
            ]


viewServicesList : Model -> List CurrentCaseInfo -> Html Msg
viewServicesList model ccs =
    let
        cases =
            List.map (viewCard model) ccs
        
        header = 
            h2 [ style "text-align" "center" ] [ text "Текущие заявки" ]

        hideMobile = 
            class "d-none d-lg-block"
    in
    div [ hideMobile ] 
        [ header
        , Card.deck
            [ Card.customListGroup cases (Card.config [])
            ]
        ]
    


viewCard : Model -> CurrentCaseInfo -> ListGroup.CustomItem Msg
viewCard model cci =
    let
        serviceType c =
            case c.cuTypeOfService of
                Just tos ->
                    case Dict.get tos model.typeOfServiceSynonym of
                        Just v ->
                            v

                        Nothing ->
                            tos

                Nothing ->
                    ""

        address c =
            Ui.addressCell c.cuBreakdownPlace

        accordTime =
            cci.cuAccordTime
                |> formatAccordTime
                |> highlightAccordTime

        formatAccordTime : String -> String
        formatAccordTime t =
            case parseTime t of
                Just ( 0, hours, minutes ) ->
                    String.fromInt hours ++ ":" ++ String.fromInt minutes

                Just ( days, hours, minutes ) ->
                    String.fromInt days
                        ++ " дн. "
                        ++ String.fromInt hours
                        ++ ":"
                        ++ String.fromInt minutes

                Nothing ->
                    t

        highlightAccordTime : String -> Html msg
        highlightAccordTime s =
            case s of
                "Опоздание" ->
                    div
                        [ style "color" "white"
                        , style "border" "1px solid red"
                        , style "padding" "3px"
                        , style "border-radius" "3px"
                        , style "background-color" "tomato"
                        , style "float" "right"
                        , style "font-weight" "bold"
                        ]
                        [ text s ]

                _ ->
                    div
                        [ style "color" "white"
                        , style "border" "1px solid #28a745"
                        , style "padding" "3px"
                        , style "border-radius" "3px"
                        , style "background-color" "#28a745"
                        , style "float" "right"
                        , style "font-weight" "bold"
                        ]
                        [ text s ]

        {- Returns: (Days, Hours, Minutes) -}
        parseTime : String -> Maybe ( Int, Int, Int )
        parseTime t =
            case String.split " " t of
                [ d, h, m ] ->
                    case List.map String.toInt [ d, h, m ] of
                        [ Just days, Just hours, Just minutes ] ->
                            if days >= 0 && hours >= 0 && (minutes >= 0 && minutes < 60) then
                                Just ( days, hours, minutes )

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    ListGroup.button
        [ ListGroup.attrs
            [ onClick (CurrentCase cci.cuServiceId)
            , if (cci.cuCaseId == model.service.caseId) && (cci.cuServiceSerial == model.service.services) then
                class "active"

              else
                class ""
            , style "border-top" "1px solid #f0f0f0"
            ]
        ]
        [ div [ style "display" "block" ]
            [ div []
                [ text <|
                    String.fromInt cci.cuCaseId
                        ++ (if cci.cuServiceSerial > 1 then
                                "/" ++ String.fromInt cci.cuServiceSerial

                            else
                                ""
                           )
                , text " "
                , b [] [ text <| serviceType cci ]
                ]
            , div [ style "text-align" "right" ] [ accordTime ]
            , address cci
            ]
        ]


viewClosingCaseForClosed : Payment -> Types.PaymentType -> Html Msg
viewClosingCaseForClosed payment paytype =
    let
        priceInputStyles =
            [ Input.attrs
                [ Spacing.ml0
                , Spacing.mr0
                , Spacing.mt3
                , Spacing.mb3
                ]
            , Input.disabled True
            ]

        descriptionInputStyles =
            [ Textarea.id "priceDescription"
            , Textarea.rows 4
            , Textarea.attrs
                [ Spacing.ml0
                , Spacing.mr0
                , Spacing.mt3
                , Spacing.mb3
                ]
            , Textarea.disabled
            ]

        viewRuamk : Float -> String -> Html msg
        viewRuamk price description =
            div []
                [ h3 [] [ text "Заявка закрыта: Оплата РАМК" ]
                , Input.text <| Input.attrs [ placeholder <| String.fromFloat price ] :: priceInputStyles
                , Textarea.textarea (Textarea.attrs [ placeholder description ] :: descriptionInputStyles)
                ]

        viewClient : String -> Html msg
        viewClient price =
            div []
                [ h3 [] [ text "Заявка закрыта: Оплата клиент" ]
                , Input.text <| Input.attrs [ placeholder price ] :: priceInputStyles
                ]

        viewMixed : Float -> String -> String -> Html msg
        viewMixed price description priceClient =
            div []
                [ h3 [] [ text "Заявка закрыта: Оплата РАМК" ]
                , Input.text <| Input.attrs [ placeholder <| String.fromFloat price ] :: priceInputStyles
                , Textarea.textarea (Textarea.attrs [ placeholder description ] :: descriptionInputStyles)
                , h3 [] [ text "Заявка закрыта: Оплата клиент" ]
                , Input.text <| Input.attrs [ placeholder <| priceClient ] :: priceInputStyles
                ]

        viewRefund : String -> Html msg
        viewRefund price =
            div []
                [ h3 [] [ text "Заявка закрыта: Оплата клиент с возмещением" ]
                , Input.text <| Input.attrs [ placeholder price ] :: priceInputStyles
                ]

        -- this is becasue we have to find out who closed the service
        unpackRUAMK : Payment -> Maybe { ruamkPrice : Float, ruamkDescription : String }
        unpackRUAMK p =
            let
                byPartner =
                    Maybe.map2
                        (\a b -> { ruamkPrice = a, ruamkDescription = b })
                        p.partnerCost
                        p.partnerCostTranscript

                byOperator =
                    Maybe.map2
                        (\a b -> { ruamkPrice = a, ruamkDescription = b })
                        p.checkCost
                        p.checkCostTranscript
            in
            case byPartner of
                Just x ->
                    Just x

                Nothing ->
                    byOperator

        viewNotEnoughParams =
            div [] [ text "Информация о закрытии не вяжется с указанным типом услуги" ]
    in
    case paytype of
        Types.RUAMK ->
            let
                needed =
                    unpackRUAMK payment
            in
            case needed of
                Just { ruamkPrice, ruamkDescription } ->
                    viewRuamk ruamkPrice ruamkDescription

                Nothing ->
                    viewNotEnoughParams

        Types.Client ->
            let
                needed =
                    payment.paidByClient
            in
            case needed of
                Just clientPrice ->
                    viewClient clientPrice

                Nothing ->
                    viewNotEnoughParams

        Types.Mixed ->
            let
                needed =
                    Maybe.map2
                        (\{ ruamkPrice, ruamkDescription } clientPrice -> ( ruamkPrice, ruamkDescription, clientPrice ))
                        (unpackRUAMK payment)
                        payment.paidByClient
            in
            case needed of
                Just ( ruamkPrice, ruamkDesc, clientPrice ) ->
                    viewMixed ruamkPrice ruamkDesc clientPrice

                Nothing ->
                    viewNotEnoughParams

        Types.Refund ->
            let
                needed =
                    payment.paidByClient
            in
            case needed of
                Just clientPrice ->
                    viewRefund clientPrice

                Nothing ->
                    viewNotEnoughParams



-- TODO: Refactor it in the same way as viewClosingCaseForClosed


viewClosingCaseField : Model -> Html Msg
viewClosingCaseField model =
    let
        priceInputStyles =
            [ Input.attrs [ placeholder "Стоимость заявки (одной суммой)*" ]
            , Input.attrs
                [ Spacing.ml0
                , Spacing.mr0
                , Spacing.mt3
                , Spacing.mb3
                ]
            ]

        descriptionInputStyles =
            [ Textarea.id "priceDescription"
            , Textarea.rows 4
            , Textarea.attrs
                [ placeholder "Расшифровка стоимости"
                , Spacing.ml0
                , Spacing.mr0
                , Spacing.mt3
                , Spacing.mb3
                ]
            ]

        button enable =
            Button.button
                [ Button.primary
                , Button.attrs [ Spacing.mt3, class "float-right", onClick CloseService ]
                , if (model.service.status == Const.serviceStatus.ok) && enable then
                    Button.disabled False

                  else
                    Button.disabled True
                ]
                [ if model.service.status == Const.serviceStatus.inProgress || model.service.status == Const.serviceStatus.ordered then
                    text "Заявка выполняется"

                  else
                    text "Закрыть заявку"
                ]
    in
    case model.closingServiceForm of
        Just form ->
            case form of
                RUAMK f ->
                    let
                        updatePriceRUAMK s =
                            setPriceRUAMK s (RUAMK f)
                                |> UpdateClosingServiceForm

                        updateDescriptionRUAMK s =
                            setDescriptionRUAMK s (RUAMK f)
                                |> UpdateClosingServiceForm
                    in
                    div []
                        [ h3 [] [ text "Оплата РАМК" ]
                        , Input.text <|
                            Input.attrs
                                [ onInput updatePriceRUAMK
                                , if f.red then
                                    class "border-danger"

                                  else
                                    class ""
                                ]
                                :: priceInputStyles
                        , Textarea.textarea (Textarea.attrs [ onInput updateDescriptionRUAMK ] :: descriptionInputStyles)
                        , button (isJust <| String.toFloat f.price)
                        ]

                Client f ->
                    let
                        updatePriceClient s =
                            setPriceClient s (Client f)
                                |> UpdateClosingServiceForm
                    in
                    div []
                        [ h3 [] [ text "Оплата клиент" ]
                        , Input.text
                            (Input.attrs
                                [ onInput updatePriceClient
                                , if f.red then
                                    class "border-danger"

                                  else
                                    class ""
                                ]
                                :: priceInputStyles
                            )
                        , button (isJust <| String.toFloat f.price)
                        ]

                Mixed f ->
                    let
                        updateRUAMKPrice s =
                            setRUAMKPriceMixed s (Mixed f)
                                |> UpdateClosingServiceForm

                        updateClientPrice s =
                            setClientPriceMixed s (Mixed f)
                                |> UpdateClosingServiceForm

                        updateRUAMKDescription s =
                            setRUAMKDescriptionMixed s (Mixed f)
                                |> UpdateClosingServiceForm
                    in
                    div []
                        [ h3 [] [ text "Оплата РАМК" ]
                        , Input.text <|
                            Input.attrs
                                [ onInput updateRUAMKPrice
                                , if f.ruamkRed && f.clientRed then
                                    class "border-danger"

                                  else
                                    class ""
                                ]
                                :: priceInputStyles
                        , Textarea.textarea (Textarea.attrs [ onInput updateRUAMKDescription ] :: descriptionInputStyles)
                        , h3 [] [ text "Оплата клиент" ]
                        , Input.text <|
                            Input.attrs
                                [ onInput updateClientPrice
                                , if f.clientRed then
                                    class "border-danger"

                                  else
                                    class ""
                                ]
                                :: priceInputStyles
                        , button <|
                            List.foldr (&&)
                                True
                                [ isJust <| String.toFloat f.priceRUAMK
                                , isJust <| String.toFloat f.priceClient
                                ]
                        ]

                Refund f ->
                    let
                        updatePriceRefund s =
                            setPriceRefund s (Refund f)
                                |> UpdateClosingServiceForm
                    in
                    div []
                        [ h3 [] [ text "Оплата клиент" ]
                        , Input.text <|
                            Input.attrs
                                [ onInput updatePriceRefund
                                , if f.red then
                                    class "border-danger"

                                  else
                                    class ""
                                ]
                                :: priceInputStyles
                        , button (isJust <| String.toFloat f.price)
                        ]

        Nothing ->
            Ui.viewSpinner "10rem"


isJust : Maybe a -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False



{-

   type ClosingCaseForm
       = RUAMK
           { price : Float
           , description : String
           }
       | Client
           { price : Float
           }
       | Mixed
           { priceRUAMK : Float
           , priceClient : Float
           }
       | Refund
           { price : Float
           }

-}


setPriceRUAMK : String -> ClosingServiceForm -> ClosingServiceForm
setPriceRUAMK n form =
    case form of
        RUAMK f ->
            RUAMK { f | price = n }

        _ ->
            form


setDescriptionRUAMK : String -> ClosingServiceForm -> ClosingServiceForm
setDescriptionRUAMK desc form =
    case form of
        RUAMK f ->
            RUAMK { f | description = desc }

        _ ->
            form


setPriceClient : String -> ClosingServiceForm -> ClosingServiceForm
setPriceClient n form =
    case form of
        Client f ->
            Client { f | price = n }

        _ ->
            form


setRUAMKPriceMixed : String -> ClosingServiceForm -> ClosingServiceForm
setRUAMKPriceMixed n form =
    case form of
        Mixed f ->
            Mixed { f | priceRUAMK = n }

        _ ->
            form


setRUAMKDescriptionMixed : String -> ClosingServiceForm -> ClosingServiceForm
setRUAMKDescriptionMixed n form =
    case form of
        Mixed f ->
            Mixed { f | descriptionRUAMK = n }

        _ ->
            form


setClientPriceMixed : String -> ClosingServiceForm -> ClosingServiceForm
setClientPriceMixed n form =
    case form of
        Mixed f ->
            Mixed { f | priceClient = n }

        _ ->
            form


setPriceRefund : String -> ClosingServiceForm -> ClosingServiceForm
setPriceRefund n form =
    case form of
        Refund f ->
            Refund { f | price = n }

        _ ->
            form
