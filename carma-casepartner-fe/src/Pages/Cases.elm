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
    }


init : PageContext -> Params.Cases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( { currentCases = []
      , currentCasesPage = 1
      , closingCases = []
      , closingCasesPage = 1
      }
    , Api.getLatestCurrentCases GetCurrentCases
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
    | GetCurrentCases (Result Http.Error (List TheCase))
    | GetClosingCases (Result Http.Error (List TheCase))
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

        GetCurrentCases result ->
            case result of
                Err _ ->
                    ( model
                    , Ports.log "Error get current latest cases"
                    , Cmd.none
                    )

                Ok currentCases ->
                    ( { model
                        | currentCases = currentCases
                        , currentCasesPage = 1
                      }
                    , Api.getLatestClosingCases GetClosingCases
                    , Cmd.none
                    )

        GetClosingCases result ->
            case result of
                Err _ ->
                    ( model
                    , Ports.log "Error get closing latest cases"
                    , Cmd.none
                    )

                Ok closingCases ->
                    ( { model
                        | closingCases = closingCases
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
                    in
                    ( { model
                        | currentCasesPage = currentCasesPage
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
                    in
                    ( { model
                        | closingCasesPage = closingCasesPage
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        CasesNextPage t ->
            case t of
                Current ->
                    let
                        currentCasesPage =
                            if
                                model.currentCasesPage
                                    < ceiling
                                        (toFloat (List.length model.currentCases)
                                            / toFloat pageSize
                                        )
                            then
                                model.currentCasesPage + 1

                            else
                                model.currentCasesPage
                    in
                    ( { model
                        | currentCasesPage = currentCasesPage
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Closing ->
                    let
                        closingCasesPage =
                            if
                                model.closingCasesPage
                                    < ceiling
                                        (toFloat (List.length model.closingCases)
                                            / toFloat pageSize
                                        )
                            then
                                model.closingCasesPage + 1

                            else
                                model.closingCasesPage
                    in
                    ( { model
                        | closingCasesPage = closingCasesPage
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
        [ button Ui.activeTabStyle
            { label = text "Текущие заявки"
            , onPress = Nothing
            }
        , button Ui.inactiveTabStyle
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


viewCasesTitle : String -> String -> CasesType -> Element Msg
viewCasesTitle title pageNumber caseType =
    row [ spacingXY 0 24 ]
        [ el [] <| text title
        , el [ width (px 32) ] <| none
        , el [ padding 8 ] <|
            button Ui.pageButtonStyle
                { label = text "<"
                , onPress = Just <| CasesPrevPage caseType
                }
        , el [ padding 8 ] <|
            button Ui.pageButtonStyle
                { label = text ">"
                , onPress = Just <| CasesNextPage caseType
                }
        , el [ padding 8 ] <| text pageNumber
        ]


viewCurrentCases : Model -> Element Msg
viewCurrentCases model =
    column [ width fill, paddingXY 100 0 ]
        [ viewCasesTitle "Текущие заявки"
            (String.fromInt model.currentCasesPage
                ++ " / "
                ++ String.fromInt
                    (ceiling
                        (toFloat (List.length model.currentCases)
                            / toFloat pageSize
                        )
                    )
            )
            Current
        , table Ui.casesTableStyle
            { data =
                model.currentCases
                    |> List.drop ((model.currentCasesPage - 1) * pageSize)
                    |> List.take pageSize
            , columns =
                [ { header = Ui.headerCell "Заявка"
                  , width = fill
                  , view = \theCase -> Ui.idCell CurrentCase theCase.id
                  }
                , { header = Ui.headerCell "Дата"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.callDate
                  }
                , { header = Ui.headerCell "Тип услуги"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.typeOfService
                  }
                , { header = Ui.headerCell "Статус"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "ОВНОУ (п)"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.accordTime
                  }
                , { header = Ui.headerCell "Остаток времени (таймер)"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.remainTime
                  }
                , { header = Ui.headerCell "Изменить статус"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "Марка/Модель"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.makeModel
                  }
                , { header = Ui.headerCell "Адрес места поломки"
                  , width = fill
                  , view = \theCase -> Ui.addressCell theCase.breakdownPlace
                  }
                , { header = Ui.headerCell "Тип оплаты"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.payType
                  }
                ]
            }
        ]


viewClosingCases : Model -> Element Msg
viewClosingCases model =
    column [ width fill, paddingXY 100 0 ]
        [ viewCasesTitle "Закрытие заявок"
            (String.fromInt model.closingCasesPage
                ++ " / "
                ++ String.fromInt (ceiling (toFloat (List.length model.closingCases) / toFloat pageSize))
            )
            Closing
        , table Ui.casesTableStyle
            { data =
                model.closingCases
                    |> List.drop ((model.closingCasesPage - 1) * pageSize)
                    |> List.take pageSize
            , columns =
                [ { header = Ui.headerCell "Заявка"
                  , width = fill
                  , view = \theCase -> Ui.idCell ClosingCase theCase.id
                  }
                , { header = Ui.headerCell "Дата"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.callDate
                  }
                , { header = Ui.headerCell "Тип услуги"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.typeOfService
                  }
                , { header = Ui.headerCell "Статус"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "ОВНОУ (п)"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.accordTime
                  }
                , { header = Ui.headerCell "Остаток времени (таймер)"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.remainTime
                  }
                , { header = Ui.headerCell "Изменить статус"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.status
                  }
                , { header = Ui.headerCell "Марка/Модель"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.makeModel
                  }
                , { header = Ui.headerCell "Адрес места поломки"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.breakdownPlace
                  }
                , { header = Ui.headerCell "Тип оплаты"
                  , width = fill
                  , view = \theCase -> Ui.cell theCase.payType
                  }
                ]
            }
        ]
