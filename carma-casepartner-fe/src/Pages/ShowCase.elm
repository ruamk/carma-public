module Pages.ShowCase exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft)
import Generated.Params as Params
import Global
import Spa.Page
import Types exposing (TheCase)
import Ui
import Utils.Spa exposing (Page, PageContext)


page : Page Params.ShowCase Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "ShowCase"
        , init = init
        , update = update
        , subscriptions = always subscriptions
        , view = view
        }



-- INIT


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
    { cases : List TheCase
    , theCase : TheCase
    , closing1 : String
    , closing2 : String
    , closing3 : String
    , closing4 : String
    , inputComment : String
    , commentFileName : String
    , comments : List Comment
    , caseStatus : Maybe CaseStatus
    }


init : PageContext -> Params.ShowCase -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( { cases =
            [ { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            , { id = "654765/1"
              , callDate ="16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            ]
      , theCase =
            { id = "654765/1"
            , callDate ="16.02.2020"
            , typeOfService = "Эвакуатор"
            , status = "Услуга оказана"
            , accordTime = "11.11.2019 17:07:14"
            , remainTime = "01:15:03"
            , makeModel = "Ford/Mondeo"
            , breakdownPlace = "Новосибирск, Красный проспект 10"
            , payType = "РАМК"
            }
      , closing1 = ""
      , closing2 = ""
      , closing3 = ""
      , closing4 = ""
      , inputComment = ""
      , commentFileName = ""
      , comments =
            [ { author = "Заполянская Ольга +74952550780"
              , action = "Передана заявка"
              , date = "15.01.2020 14:50:21"
              , result = "Перевод действия"
              , service = "Эвакуатор"
              }
            , { author = "Заполянская Ольга +74952550780"
              , action = "Передана заявка"
              , date = "15.01.2020 14:50:21"
              , result = "Перевод действия"
              , service = "Эвакуатор"
              }
            , { author = "Заполянская Ольга +74952550780"
              , action = "Передана заявка"
              , date = "15.01.2020 14:50:21"
              , result = "Перевод действия"
              , service = "Эвакуатор"
              }
            , { author = "Заполянская Ольга +74952550780"
              , action = "Передана заявка"
              , date = "15.01.2020 14:50:21"
              , result = "Перевод действия"
              , service = "Эвакуатор"
              }
            , { author = "Заполянская Ольга +74952550780"
              , action = "Передана заявка"
              , date = "15.01.2020 14:50:21"
              , result = "Перевод действия"
              , service = "Эвакуатор"
              }
            ]
      , caseStatus = Nothing
      }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


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


update : PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update context msg model =
    case msg of
        Cases ->
            ( model
            , Cmd.none
            , Spa.Page.send <| Global.Cases context.global.username
            )

        SearchCases ->
            ( model
            , Cmd.none
            , Spa.Page.send <| Global.SearchCases
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

        SelectStatus s ->
            ( { model | caseStatus = s }
            , Cmd.none
            , Cmd.none
            )



{-
   _ ->
       ( model
       , Cmd.none
       , Cmd.none
       )
-}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : PageContext -> Model -> Element Msg
view context model =
    Ui.page context.global.username
        [ button Ui.inactiveTabStyle
            { label = text "Текущие заявки"
            , onPress = Just Cases
            }
        , button Ui.inactiveTabStyle
            { label = text "Поиск заявок"
            , onPress = Just SearchCases
            }
        ]
    <|
        row [ height fill, width fill ]
            [ viewCases model.cases
            , viewCasePanel model
            ]


viewCases : List TheCase -> Element msg
viewCases cases =
    column
        [ width (px 200)
        , height fill
        , scrollbarY
        , paddingXY 8 16
        , spacing 4
        ]
    <|
        List.map viewCase cases


nameStyle =
    [ Font.size 14, Font.semiBold ]


valueStyle =
    [ Font.size 14 ]


name t =
    el nameStyle <| text t


value t =
    el valueStyle <| text t


field n v =
    row [ padding 4 ]
        [ name <| n ++ ": "
        , value v
        ]


viewCase : TheCase -> Element msg
viewCase theCase =
    let
        columnStyle =
            [ spacing 4
            , padding 4
            , width fill
            , Bd.width 1
            , Bd.rounded 8
            , Bd.color Ui.colors.darkgray
            ]
    in
    column columnStyle
        [ field "заявка" theCase.id
        , field "тип услуги" theCase.typeOfService
        , field "статус" theCase.status
        ]


viewCasePanel : Model -> Element Msg
viewCasePanel model =
    column [ alignTop, width fill ]
        [ viewCaseVerbose model
        , Ui.horizontalLine
        , viewCaseStatus model
        , Ui.horizontalLine
        , viewCaseClosing model
        , Ui.horizontalLine
        , viewCaseCommentsPanel model
        ]


viewCaseVerbose : Model -> Element msg
viewCaseVerbose model =
    row []
        [ column [ padding 16, alignTop ]
            [ field "Номер заявки" model.theCase.id
            , field "Вид помощи" model.theCase.typeOfService
            , field "Клиент" ""
            , field "Телефон клиента" ""
            , field "Адрес начала работы" ""
            , field "Адрес окончания работы" ""
            , field "Желаемая дата оказания услуг" ""
            , field "Факт. время оказания услуг" ""
            , field "Время окончания работы" ""
            ]
        , column [ padding 16, alignTop ]
            [ field "Марка и модель авто" <| model.theCase.makeModel
            , field "Гос. номер" "не указан"
            , field "Сложность погрузки" ""
            , field "Перепробег за МКАД" ""
            , field "Простой" ""
            , field "Переадресация" ""
            , field "Стоимость услуги" ""
            , field "KPI" ""
            ]
        ]


viewCaseStatus : Model -> Element Msg
viewCaseStatus model =
    column [ centerX ]
        [ field "Статус заявки" "услуга оказана"
        , row []
            [ Input.radio []
                { label = labelLeft [] <| text "Изменить статус:"
                , selected = Just model.caseStatus
                , onChange = SelectStatus
                , options =
                    [ Input.option (Just TowageArrived) <| text "Эвакуатор на месте"
                    , Input.option (Just TowageAwaiting) <| text "Эвакуатор в пути"
                    ]
                }
            ]
        ]


viewCaseClosing : Model -> Element Msg
viewCaseClosing model =
    let
        inputStyle =
            [ Font.size 14 ]
    in
    column [ padding 16 ]
        [ field "Закрытие заявки" ""
        , row []
            [ Input.text inputStyle
                { label = labelLeft [] <| none
                , text = model.closing1
                , placeholder = Nothing
                , onChange = Closing1
                }
            , Input.text inputStyle
                { label = labelLeft [] <| none
                , text = model.closing2
                , placeholder = Nothing
                , onChange = Closing2
                }
            , Input.text inputStyle
                { label = labelLeft [] <| none
                , text = model.closing3
                , placeholder = Nothing
                , onChange = Closing3
                }
            , Input.text inputStyle
                { label = labelLeft [] <| none
                , text = model.closing4
                , placeholder = Nothing
                , onChange = Closing4
                }
            ]
        ]


commentStyle : List (Attribute msg)
commentStyle =
    [ spacing 4
    , padding 4
    , width fill
    , Bd.width 1
    , Bd.rounded 8
    , Bd.color Ui.colors.darkgray
    ]


viewComment : Comment -> Element msg
viewComment comment =
    column commentStyle
        [ el
            [ Font.semiBold
            , Font.size 14
            ]
          <|
            text <|
                comment.date
                    ++ " "
                    ++ comment.author
        , field "Действие" comment.action
        , field "Результат" comment.result
        , field "Услуга" comment.service
        ]


viewCaseCommentsPanel : Model -> Element Msg
viewCaseCommentsPanel model =
    column [ width fill, padding 16 ]
        [ Input.multiline
            [ centerX
            , width fill
            , height (px 100)
            , Font.size 14
            ]
            { onChange = InputComment
            , text = model.inputComment
            , placeholder = Just <| Input.placeholder [] <| text "Добавить комментарий..."
            , label = labelLeft [] <| none
            , spellcheck = False
            }
        , row [ paddingXY 4 8, spacingXY 8 8, width fill ]
            [ Ui.button ( "Добавить комментарий", Nothing )
            , el [ width fill ] <| none
            , Input.text [ Font.size 14, spacingXY 8 8 ]
                { label = labelLeft [] <| none
                , text = model.commentFileName
                , placeholder = Just <| Input.placeholder [] <| text "Добавить файл"
                , onChange = CommentFileName
                }
            , Ui.button ( "🗁", Nothing )
            , Ui.button ( "⭳", Nothing )
            ]
        , column
            [ scrollbarY
            , width fill
            , height fill
            , spacing 4
            , paddingXY 8 16
            ]
          <|
            List.map viewComment model.comments
        ]
