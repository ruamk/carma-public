module Pages.Cases exposing (Model, Msg, page)

import Api
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input exposing (button)
import Generated.Params as Params
import Global
import Http
import Ports
import Spa.Page
import Types exposing (TheCase)
import Ui
import Utils.Spa exposing (Page, PageContext)


page : Page Params.Cases Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Текущие заявки"
        , init = init
        , update = update
        , subscriptions = always subscriptions
        , view = view
        }



-- INIT


pageSize : Int
pageSize =
    10


type alias Model =
    { currentCases : List TheCase
    , currentCasesPage : Int
    , closingCases : List TheCase
    , closingCasesPage : Int
    , latestCases : List TheCase
    }


init : PageContext -> Params.Cases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( { currentCases = []
      , currentCasesPage = 1
      , closingCases = []
      , closingCasesPage = 1
      , latestCases = []
      }
    , Api.getLatestCases GetCases
    , Cmd.none
    )



-- UPDATE


type CasesType
    = Current
    | Closing


type Msg
    = CurrentCase String
    | ClosingCase String
    | SearchCases
    | GetCases (Result Http.Error (List TheCase))
    | CasesPrevPage CasesType
    | CasesNextPage CasesType


update : PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update context msg model =
    case msg of
        CurrentCase caseId ->
            ( model
            , Ports.log <| "CurrentCase " ++ caseId
            , Spa.Page.send <| Global.ShowCase caseId
            )

        ClosingCase caseId ->
            ( model
            , Ports.log <| "ClosingCase " ++ caseId
            , Spa.Page.send <| Global.ShowCase caseId
            )

        SearchCases ->
            ( model
            , Cmd.none
            , Spa.Page.send Global.SearchCases
            )

        GetCases result ->
            case result of
                Err _ ->
                    ( model
                    , Ports.log "Error get latest cases"
                    , Cmd.none
                    )

                Ok cases ->
                    ( { model
                        | latestCases = cases
                        , currentCases = List.take pageSize cases
                        , currentCasesPage = 1
                        , closingCases = List.take pageSize cases
                        , closingCasesPage = 1
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        CasesPrevPage t ->
            case t of
                Current ->
                    let
                        currentCasesPage =
                            if model.currentCasesPage > 1 then
                                model.currentCasesPage - 1

                            else
                                1

                        currentCases =
                            List.take pageSize <|
                                List.drop ((currentCasesPage - 1) * pageSize) model.latestCases
                    in
                    ( { model
                        | currentCasesPage = currentCasesPage
                        , currentCases = currentCases
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Closing ->
                    let
                        closingCasesPage =
                            if model.closingCasesPage > 1 then
                                model.closingCasesPage - 1

                            else
                                1

                        closingCases =
                            List.take pageSize <|
                                List.drop ((closingCasesPage - 1) * pageSize) model.latestCases
                    in
                    ( { model
                        | closingCasesPage = closingCasesPage
                        , closingCases = closingCases
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        CasesNextPage t ->
            case t of
                Current ->
                    let
                        currentCasesPage =
                            if model.currentCasesPage < List.length model.latestCases // pageSize then
                                model.currentCasesPage + 1

                            else
                                model.currentCasesPage

                        currentCases =
                            List.take pageSize <|
                                List.drop ((currentCasesPage - 1) * pageSize) model.latestCases
                    in
                    ( { model
                        | currentCasesPage = currentCasesPage
                        , currentCases = currentCases
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Closing ->
                    let
                        closingCasesPage =
                            if model.closingCasesPage < List.length model.latestCases // pageSize then
                                model.closingCasesPage + 1

                            else
                                model.closingCasesPage

                        closingCases =
                            List.take pageSize <|
                                List.drop ((closingCasesPage - 1) * pageSize) model.latestCases
                    in
                    ( { model
                        | closingCasesPage = closingCasesPage
                        , closingCases = closingCases
                      }
                    , Cmd.none
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
        [ button [ Font.semiBold, Font.color Ui.colors.white ]
            { label = text "Текущие заявки"
            , onPress = Nothing
            }
        , button [ Font.underline, Font.color Ui.colors.gray ]
            { label = text "Поиск заявок"
            , onPress = Just SearchCases
            }
        ]
    <|
        column [ alignTop ]
            [ viewCurrentCases model
            , el [ height (px 32) ] none
            , viewClosingCases model
            ]


viewCasesTitle : String -> Int -> CasesType -> Element Msg
viewCasesTitle title pageNumber caseType =
    row [ spacingXY 0 24 ]
        [ el [] <| text title
        , el [ width (px 32) ] <| none
        , el [ padding 8 ] <|
            button []
                { label = text "<"
                , onPress = Just <| CasesPrevPage caseType
                }
        , el [ padding 8 ] <|
            button []
                { label = text <| String.fromInt pageNumber
                , onPress = Nothing
                }
        , el [ padding 8 ] <|
            button []
                { label = text ">"
                , onPress = Just <| CasesNextPage caseType
                }
        ]


viewCurrentCases : Model -> Element Msg
viewCurrentCases model =
    column []
        [ viewCasesTitle "Текущие заявки" model.currentCasesPage Current
        , row []
            [ table Ui.casesTableStyle
                { data = model.currentCases
                , columns =
                    [ { header = Ui.headerCell "Заявка"
                      , width = shrink
                      , view = \theCase -> Ui.idCell CurrentCase theCase.id
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
            ]
        ]


viewClosingCases : Model -> Element Msg
viewClosingCases model =
    column []
        [ viewCasesTitle "Закрытие заявок" model.closingCasesPage Closing
        , row []
            [ table Ui.casesTableStyle
                { data = model.closingCases
                , columns =
                    [ { header = Ui.headerCell "Заявка"
                      , width = shrink
                      , view = \theCase -> Ui.idCell ClosingCase theCase.id
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
            ]
        ]
