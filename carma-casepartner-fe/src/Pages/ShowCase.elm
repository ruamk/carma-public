module Pages.ShowCase exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Chat as Chat
import Const as Const
import Dict as Dict
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Generated.Route as Route
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Time as Time
import Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , ServiceDescription
        , emptyServiceDescription
        )
import Ui exposing (empty)
import Utils exposing (formatTime)


type alias Flags =
    ()


type CaseStatus
    = TowageArrived
    | TowageAwaiting


type alias Comment =
    { author : String
    , action : String
    , date : String
    , result : String
    , service : String
    }


type alias Model =
    { service : ServiceDescription
    , closing1 : String
    , closing2 : String
    , closing3 : String
    , closing4 : String
    , inputComment : String
    , commentFileName : String
    , caseStatus : Maybe CaseStatus
    , navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , commentsDownloaded : Bool
    , comments : List CaseComment
    , statusButton2 : { label : String, disabled : Bool, disabledTime : Int, message : Maybe Msg }
    , modalVisibility : Modal.Visibility
    , messageToast : MessageToast Msg
    }


type Msg
    = Cases
    | SearchCases
    | Closing1 String
    | Closing2 String
    | Closing3 String
    | Closing4 String
    | InputComment String
    | CommentFileName String
    | ServiceDownloaded (Result Http.Error ServiceDescription)
    | CommentsDownloaded (Result Http.Error (List CaseComment))
    | AddComment
    | AddCommentResponse (Result Http.Error Int)
    | NavbarMsg Navbar.State
    | UsermenuMsg Dropdown.State
    | InPlace -- На месте
    | ServicePerformed -- Услуга оказана
    | UpdateStatus (Result Http.Error String) -- Результат изменения статуса
    | UpdateComments (Result Http.Error Int) -- Услуга оказана (ответ)
    | Logout
    | ModalClose
    | UpdateCustomMessageToast (MessageToast Msg)
    | Chat String
    | Tick Time.Posix



{- | buttonDisabledTime in seconds -}


buttonDisabledTime : Int
buttonDisabledTime =
    5


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
      , inputComment = ""
      , commentFileName = ""
      , caseStatus = Nothing
      , navbarState = navbarState
      , usermenuState = Dropdown.initialState
      , commentsDownloaded = False
      , comments = []
      , statusButton2 =
            { label = ""
            , disabled = True
            , disabledTime = 0
            , message = Nothing
            }
      , modalVisibility = Modal.hidden
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
update global msg model =
    case msg of
        -- Entry point
        NavbarMsg state ->
            ( { model | navbarState = state }
            , Api.getService global.serviceId ServiceDownloaded
            , Cmd.none
            )

        ServiceDownloaded result ->
            case result of
                Err e ->
                    let
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
                        , statusButton2 = statusButton2
                      }
                    , Api.getServiceComments global.serviceId CommentsDownloaded
                    , Cmd.none
                    )

        CommentsDownloaded result ->
            case result of
                Err e ->
                    let
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
                    , Chat.connectToCase model.service.caseId
                    , Cmd.none
                    )

        Cases ->
            ( model
            , Cmd.none
            , Global.navigate Route.Cases
            )

        SearchCases ->
            ( model
            , Cmd.none
            , Global.navigate Route.SearchCases
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
                s =
                    model.statusButton2
            in
            ( { model
                | statusButton2 =
                    if model.statusButton2.disabledTime > 0 then
                        { s | disabledTime = model.statusButton2.disabledTime - 1 }

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

                Err err ->
                    let
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

        ModalClose ->
            ( { model | modalVisibility = Modal.hidden }
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


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Dropdown.subscriptions model.usermenuState UsermenuMsg
        , Chat.caseReceiver Chat
        , Time.every 1000 Tick
        ]


formatServiceSerial : Model -> String
formatServiceSerial model =
    let
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
            , usermenuMsg = UsermenuMsg
            , navbarState = model.navbarState
            , usermenuState = model.usermenuState
            , username = global.username
            , buttons =
                [ ( False, Cases, "Текущие заявки" )
                , ( False, SearchCases, "Поиск заявок" )
                ]
            }
          <|
            div []
                [ viewCasePanel model
                , div []
                    [ model.messageToast
                        |> MessageToast.overwriteContainerAttributes
                            [ style "top" "20px"
                            , style "bottom" "auto"
                            , style "right" "20px"
                            ]
                        |> MessageToast.view
                    ]
                ]
        ]
    }


viewModal : Model -> Html Msg
viewModal model =
    Modal.config ModalClose
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Внимание" ]
        |> Modal.body [] [ p [] [ text "Услуга уже оказана" ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick ModalClose ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view model.modalVisibility


nameStyle =
    [ class "name" ]


valueStyle =
    [ class "value" ]


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


viewCasePanel : Model -> Html Msg
viewCasePanel model =
    viewCaseVerbose model


viewCaseVerbose : Model -> Html Msg
viewCaseVerbose model =
    let
        c =
            model.service

        name t =
            div nameStyle [ text t ]

        value t =
            div valueStyle [ t ]

        field n v =
            Grid.row [ Row.attrs [ Spacing.p1 ] ]
                [ Grid.col [ Col.sm5 ]
                    [ name <| n ++ ": " ]
                , Grid.col [ Col.sm7 ]
                    [ value v ]
                ]

        optionalField n v =
            if String.isEmpty v then
                br [] []

            else
                field n <| text v

        caseId =
            formatServiceSerial model

        vin =
            case c.vin of
                Just v ->
                    v

                Nothing ->
                    "отсутствует"

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
    in
    Grid.row [ Row.attrs [ Spacing.p1 ] ]
        (if model.service.caseId == 0 then
            [ Grid.col [ Col.textAlign Text.alignXsCenter ]
                [ Ui.viewSpinner "10rem"
                ]
            ]

         else
            [ Grid.col [ Col.sm2 ] []
            , Grid.col [ Col.attrs [ style "background-color" Ui.colors.casesBg ], Col.sm7 ]
                [ h2 [ class "text-center" ] [ text <| "Номер заявки: " ++ caseId ]
                , Grid.row []
                    [ Grid.col []
                        [ field "Вид помощи" <| text c.serviceType
                        , field "Клиент" <| text c.client
                        , field "Телефон клиента" <| text c.clientPhone
                        , br [] []
                        , field "Адрес начала работы" <| text c.firstAddress
                        , if c.serviceType == "Техпомощь" then
                            div [] []

                          else
                            optionalField "Адрес доставки" <| c.lastAddress
                        , field "Желаемая дата оказания услуг" <|
                            text <|
                                formatTime_ c.expectedServiceStart
                        , field "Факт. время оказания услуг" <|
                            text <|
                                formatTime_ c.factServiceStart
                        , field "Время окончания работы" <|
                            text <|
                                formatTime_ c.factServiceEnd
                        ]
                    , Grid.col []
                        [ field "Марка/Модель" <| text c.makeModel
                        , field "Гос. номер" <| text c.plateNumber
                        , field "VIN" <| text <| String.toUpper vin

                        -- , field "Сложность погрузки" <| asUl c.loadingDifficulties
                        , field "Межгород, км" <| text c.suburbanMilage
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
                        ]
                    , Grid.col [ Col.attrs [ class "text-center" ] ]
                        [ div [] [ text "Изменить статус" ]
                        , let
                            width =
                                style "width" "150px"
                          in
                          div []
                            [ {- Button.button
                                     [ Button.primary
                                     , Button.attrs [ Spacing.m3, width ]
                                     ]
                                     [ text "Опоздание" ]
                                 ,
                              -}
                              Button.button
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
                            , viewModal model
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
                                [ Form.col [ Col.sm4, Col.attrs [ Spacing.pt2 ] ]
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
                                , Form.col [ Col.sm4, Col.attrs [ Spacing.pt2 ] ] []
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
                    [ Input.text
                        [ Input.attrs [ placeholder "Стоимость заявки (одной суммой)" ]
                        , Input.attrs [ Spacing.ml0, Spacing.mr0, Spacing.mt3, Spacing.mb3 ]
                        ]
                    , Textarea.textarea
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
                    , Button.button
                        [ Button.primary, Button.attrs [ class "float-right", Spacing.mt3 ] ]
                        [ text "Закрыть заявку" ]
                    ]
                ]
            ]
        )


viewDetails : CaseComment -> List (Html Msg)
viewDetails { details } =
    let
        field n v =
            [ b [] [ text <| n ++ ": " ]
            , text v
            ]

        fieldOrHide n v =
            case v of
                Just v_ ->
                    field n v_

                Nothing ->
                    []

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
                    [ field "Событие AVAYA" aeType ]
                        ++ (if List.isEmpty aeInterLocutors then
                                []

                            else
                                [ field "Второй абонент" <| String.join " " aeInterLocutors ]
                           )
                        ++ [ field "Идентификатор звонка" aeCall ]

                Just (CaseCommentCall { callType }) ->
                    [ field "Звонок" callType ]

                Just (CaseCommentComment { commentText }) ->
                    [ field "Комментарий" commentText ]

                Just (CaseCommentPartnerCancel _) ->
                    [ field "Отказ партнёра" "" ]

                Just (CaseCommentPartnerDelay params) ->
                    let
                        { partnerName, serviceLabel, delayMinutes, delayConfirmed } =
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
                    , field "Опоздание партнёра" partnerName
                    , fieldOrHide "Время опоздания" time
                    , fieldOrHide "Опоздание согласовано" delayConfirmed
                    ]

                Just (CaseCommentSmsForPartner { msgText, sender, phone, deliveryStatus }) ->
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
                        [ Grid.row []
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
                        ]
                            ++ viewDetails l
                )
                model.comments

    else
        Grid.row []
            [ Grid.col [ Col.textAlign Text.alignXsCenter ] <|
                [ Ui.viewSpinner "10rem" ]
            ]
