module Pages.SearchCases exposing (Model, Msg, page)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Generated.Params as Params
import Global
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , navbarState : Navbar.State
    }


init : PageContext -> Params.SearchCases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
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
      , navbarState = navbarState
      }
    , navbarCmd
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
    | NavbarMsg Navbar.State


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
            , Ports.log <| "Search by breakdown place " ++ place
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.none
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : PageContext -> Model -> Html Msg
view context model =
    Ui.page NavbarMsg
        model.navbarState
        context.global.username
        [ ( False, Cases, "Текущие заявки" )
        , ( True, NavbarMsg model.navbarState, "Поиск заявок" )
        ]
    <|
        Grid.row []
            [ Grid.col []
                [ viewCases model.cases
                , viewSearchForm model
                ]
            ]


viewCases : List CaseInfo -> Html Msg
viewCases data =
    Table.table
        { options = [ Table.hover, Table.bordered ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Заявка" ]
                , Table.th [] [ text "Дата" ]
                , Table.th [] [ text "Тип услуги" ]
                , Table.th [] [ text "Статус" ]
                , Table.th [] [ text "ОВНОУ (п)" ]
                , Table.th [] [ text "Остаток времени (таймер)" ]
                , Table.th [] [ text "Изменить статус" ]
                , Table.th [] [ text "Марка/Модель" ]
                , Table.th [] [ text "Адрес места поломки" ]
                , Table.th [] [ text "Тип оплаты" ]
                ]
        , tbody =
            Table.tbody [] <|
                List.map (Table.tr []) <|
                    List.map
                        (\theCase ->
                            [ Table.td [] [ Ui.idCell Case theCase.id theCase.services ]
                            , Table.td [] [ Ui.cell theCase.callDate ]
                            , Table.td [] [ Ui.cell theCase.typeOfService ]
                            , Table.td [] [ Ui.cell theCase.status ]
                            , Table.td [] [ Ui.cell theCase.accordTime ]
                            , Table.td [] [ Ui.cell theCase.remainTime ]
                            , Table.td [] [ Ui.cell theCase.status ]
                            , Table.td [] [ Ui.cell theCase.makeModel ]
                            , Table.td [] [ Ui.cell theCase.breakdownPlace ]
                            , Table.td [] [ Ui.cell theCase.payType ]
                            ]
                        )
                        data
        }


viewSearchForm : Model -> Html Msg
viewSearchForm model =
    Form.form []
        [ Form.row []
            [ Form.colLabel [] [ text "Номер кейса:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput CaseId ], Input.value model.search_caseId ]
                ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Госномер:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput PlateNumber ], Input.value model.search_plateNumber ]
                ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Дата звонка:" ]
            , Form.col []
                [ Input.date [ Input.attrs [ onInput CallDateStart ], Input.value model.search_callDateStart ] ]
            , Form.col []
                [ Input.date [ Input.attrs [ onInput CallDateEnd ], Input.value model.search_callDateEnd ] ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Телефоны:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput Phones ], Input.value model.search_phones ] ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Марка:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput Make ], Input.value model.search_make ] ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Услуга:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput TypeOfService ], Input.value model.search_typeOfService ] ]
            ]
        , Form.row []
            [ Form.colLabel [] [ text "Адрес места поломки:" ]
            , Form.col []
                [ Input.text [ Input.attrs [ onInput BreakdownPlace ], Input.value model.search_breakdownPlace ] ]
            ]
        ]
