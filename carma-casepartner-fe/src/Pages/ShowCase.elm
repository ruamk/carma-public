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
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
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
import Page exposing (Document, Page)
import Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , ServiceDescription
        , emptyServiceDescription
        )
import Ui
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
    , statusButton2 : { label : String, disabled : Bool, message : Maybe Msg }
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
    | SelectStatus (Maybe CaseStatus)
    | ServiceDownloaded (Result Http.Error ServiceDescription)
    | CommentsDownloaded (Result Http.Error (List CaseComment))
    | AddComment
    | AddCommentResponse (Result Http.Error Int)
    | NavbarMsg Navbar.State
    | UsermenuMsg Dropdown.State
    | InPlace -- На месте
    | ServicePerformed -- Услуга оказана
    | ServicePerformedResponse (Result Http.Error Int) -- Услуга оказана (ответ)
    | Logout


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
      , statusButton2 = { label = "", disabled = True, message = Nothing }
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
                    ( model
                    , let
                        a =
                            Debug.log "Error get case in showcase "
                                [ String.fromInt global.serviceId ]
                      in
                      Cmd.none
                    , Cmd.none
                    )

                Ok service ->
                    let
                        statusButton2 =
                            if service.status == Const.serviceStatus.ordered then
                                { label = "На месте"
                                , disabled = False
                                , message = Just InPlace
                                }

                            else if service.status == Const.serviceStatus.inProgress then
                                { label = "Услуга выполнена"
                                , disabled = False
                                , message = Just ServicePerformed
                                }

                            else
                                { label = "Услуга оказана"
                                , disabled = True
                                , message = Nothing
                                }
                    in
                    ( { model
                        | service = service
                        , statusButton2 = statusButton2
                      }
                    , Api.getCaseComments service.caseId CommentsDownloaded
                    , Cmd.none
                    )

        CommentsDownloaded result ->
            case result of
                Err e ->
                    ( model
                    , let
                        a =
                            Debug.log "error get comments in showcase "
                                [ e ]
                      in
                      Cmd.none
                    , Cmd.none
                    )

                Ok comments ->
                    ( { model
                        | comments = comments
                        , commentsDownloaded = True
                      }
                    , Cmd.none
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
            , let
                a =
                    Debug.log "Add comment: " [ model.inputComment ]
              in
              Api.postServiceComment model.service.caseId
                { comment = model.inputComment }
                AddCommentResponse
            , Cmd.none
            )

        AddCommentResponse result ->
            ( model
            , let
                a =
                    Debug.log "add comment response: " [ result ]
              in
              Api.getCaseComments model.service.caseId CommentsDownloaded
            , Cmd.none
            )

        SelectStatus s ->
            ( { model | caseStatus = s }
            , Cmd.none
            , Cmd.none
            )

        UsermenuMsg state ->
            ( { model | usermenuState = state }
            , Cmd.none
            , Cmd.none
            )

        InPlace ->
            ( model
            , let
                _ =
                    Debug.log "InPlace" []
              in
              Cmd.none
            , Cmd.none
            )

        ServicePerformed ->
            ( model
            , let
                _ =
                    Debug.log "Партнер выполнил услугу: " []
              in
              Api.postServiceComment model.service.caseId
                { comment = "Партнёр выполнил услугу" }
                ServicePerformedResponse
            , Cmd.none
            )

        ServicePerformedResponse result ->
            ( model
            , let
                _ =
                    Debug.log "Партнёр выполнил услугу (ответ): " [ result ]
              in
              Api.getCaseComments model.service.caseId CommentsDownloaded
            , Cmd.none
            )

        Logout ->
            ( model
            , Cmd.none
            , Global.logout
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch [ Dropdown.subscriptions model.usermenuState UsermenuMsg ]


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
                ]
        ]
    }


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
                            [ Button.button
                                [ Button.primary
                                , Button.attrs [ Spacing.m3, width ]
                                ]
                                [ text "Опоздание" ]
                            , Button.button
                                [ Button.primary
                                , Button.disabled model.statusButton2.disabled
                                , Button.attrs
                                    ([ Spacing.m3
                                     , width
                                     ]
                                        ++ (case model.statusButton2.message of
                                                Just m ->
                                                    [ onClick m ]

                                                _ ->
                                                    []
                                           )
                                    )
                                ]
                                [ text model.statusButton2.label ]
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
                    ]
                        ++ (case serviceLabel of
                                Just serviceLabel_ ->
                                    [ field "Услуга" serviceLabel_ ]

                                Nothing ->
                                    []
                           )
                        ++ (case comment of
                                Just comment_ ->
                                    [ field "Комментарий" comment_ ]

                                Nothing ->
                                    []
                           )

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

                        time =
                            case delayMinutes of
                                Just mm ->
                                    let
                                        hours =
                                            mm // 60

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
