module Pages.SearchCases exposing (Model, Msg, page)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft)
import Generated.Params as Params
import Global
import Ports
import Spa.Page
import Types exposing (CaseInfo)
import Ui
import Utils.Spa exposing (Page, PageContext)


page : Page Params.SearchCases Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Поиск заявок"
        , init = init
        , update = update
        , subscriptions = always subscriptions
        , view = view
        }



-- INIT


type alias SearchForm =
    { caseId : Int
    , plateNumber : String
    , callDateStart : String
    , callDateEnd : String
    , phones : String
    , make : String
    , typeOfService : String
    , breakdownPlace : String
    }


type alias Model =
    { cases : List CaseInfo
    , search_caseId : String
    , search_plateNumber : String
    , search_callDateStart : String
    , search_callDateEnd : String
    , search_phones : String
    , search_make : String
    , search_typeOfService : String
    , search_breakdownPlace : String
    }


init : PageContext -> Params.SearchCases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( { cases =
            [ { id = 654765
              , services = 1
              , callDate = "16.02.2020"
              , typeOfService = "Эвакуатор"
              , status = "Услуга оказана"
              , accordTime = "11.11.2019 17:07:14"
              , remainTime = "01:15:03"
              , makeModel = "Ford/Mondeo"
              , breakdownPlace = "Новосибирск, Красный проспект 10"
              , payType = "РАМК"
              }
            ]
      , search_caseId = ""
      , search_plateNumber = ""
      , search_callDateStart = ""
      , search_callDateEnd = ""
      , search_phones = ""
      , search_make = ""
      , search_typeOfService = ""
      , search_breakdownPlace = ""
      }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = Case Int
    | Cases
    | CaseId String -- номер кейса
    | PlateNumber String -- госномер
    | CallDateStart String -- дата начала звонка
    | CallDateEnd String -- дата окончания звонка
    | Phones String -- телефоны
    | Make String -- марка
    | TypeOfService String -- тип услуги
    | BreakdownPlace String -- адрес места поломки


update : PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update context msg model =
    case msg of
        Case caseId ->
            ( model
            , Ports.log <| "SearchCases Case " ++ String.fromInt caseId
            , Spa.Page.send <| Global.ShowCase caseId
            )

        Cases ->
            ( model
            , Cmd.none
            , Spa.Page.send <| Global.Cases context.global.username
            )

        CaseId caseId ->
            ( { model | search_caseId = caseId }
            , Ports.log <| "Search by case number " ++ caseId
            , Cmd.none
            )

        PlateNumber plateNumber ->
            ( { model | search_plateNumber = plateNumber }
            , Ports.log <| "Search by plate number " ++ plateNumber
            , Cmd.none
            )

        CallDateStart date ->
            ( { model | search_callDateStart = date }
            , Ports.log <| "Search by call date start " ++ date
            , Cmd.none
            )

        CallDateEnd date ->
            ( { model | search_callDateEnd = date }
            , Ports.log <| "Search by call date end " ++ date
            , Cmd.none
            )

        Phones phones ->
            ( { model | search_phones = phones }
            , Ports.log <| "Search by phones " ++ phones
            , Cmd.none
            )

        Make make ->
            ( { model | search_make = make }
            , Ports.log <| "Search by make " ++ make
            , Cmd.none
            )

        TypeOfService typeOfService ->
            ( { model | search_typeOfService = typeOfService }
            , Ports.log <| "Search by type of service " ++ typeOfService
            , Cmd.none
            )

        BreakdownPlace place ->
            ( { model | search_breakdownPlace = place }
            , Ports.log <| "Saerch by breakdown place " ++ place
            , Cmd.none
            )



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
        , button Ui.activeTabStyle
            { label = text "Поиск заявок"
            , onPress = Nothing
            }
        ]
    <|
        row [ padding 16 ]
            [ viewCases model.cases
            , viewSearchForm model
            ]


viewCases : List CaseInfo -> Element Msg
viewCases data =
    el [ alignTop ] <|
        table Ui.casesTableStyle
            { data = data
            , columns =
                [ { header = Ui.headerCell "Заявка"
                  , width = shrink
                  , view = \theCase -> Ui.idCell Case theCase.id theCase.services
                  }
                , { header = Ui.headerCell "Дата"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.callDate
                  }
                , { header = Ui.headerCell "Тип услуги"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.typeOfService
                  }
                , { header = Ui.headerCell "Статус"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "ОВНОУ (п)"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.accordTime
                  }
                , { header = Ui.headerCell "Остаток времени (таймер)"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.remainTime
                  }
                , { header = Ui.headerCell "Изменить статус"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "Марка/Модель"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.makeModel
                  }
                , { header = Ui.headerCell "Адрес места поломки"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.breakdownPlace
                  }
                , { header = Ui.headerCell "Тип оплаты"
                  , width = shrink
                  , view = \theCase -> Ui.cell theCase.payType
                  }
                ]
            }


viewSearchForm : Model -> Element Msg
viewSearchForm model =
    let
        font =
            [ Font.size 12 ]

        leftLabel t =
            labelLeft [ centerY ] <| text t

        textStyle =
            font ++ [ width (px 205) ]

        dateStyle =
            font ++ [ width (px 100) ]

        datePlaceholder =
            Just <| Input.placeholder [] <| text "ГГГГ-ММ-ДД"
    in
    column [ paddingXY 16 0, alignTop, spacing 8, width (px 400) ]
        [ el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Номер кейса:"
                , text = model.search_caseId
                , placeholder = Nothing
                , onChange = CaseId
                }
        , el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Госномер:"
                , text = model.search_plateNumber
                , placeholder = Nothing
                , onChange = PlateNumber
                }
        , row [ alignRight ]
            [ el font <| text "Дата звонка:"
            , Input.text dateStyle
                { label = labelLeft [] <| none
                , text = model.search_callDateStart
                , placeholder = datePlaceholder
                , onChange = CallDateStart
                }
            , Input.text dateStyle
                { label = labelLeft [] <| none
                , text = model.search_callDateEnd
                , placeholder = datePlaceholder
                , onChange = CallDateEnd
                }
            ]
        , el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Телефоны:"
                , text = model.search_phones
                , placeholder = Nothing
                , onChange = Phones
                }
        , el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Марка:"
                , text = model.search_make
                , placeholder = Nothing
                , onChange = Make
                }
        , el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Услуга:"
                , text = model.search_typeOfService
                , placeholder = Nothing
                , onChange = TypeOfService
                }
        , el [ alignRight ] <|
            Input.text textStyle
                { label = leftLabel "Адрес места поломки:"
                , text = model.search_breakdownPlace
                , placeholder = Nothing
                , onChange = BreakdownPlace
                }
        ]
