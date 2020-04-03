module Pages.Cases exposing (Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Generated.Params as Params
import Global
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOver)
import Http
import ISO8601 as ISO8601
import Ports
import Spa.Page
import Task
import Time
import Types exposing (CaseInfo)
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
    { currentCases : List CaseInfo
    , currentCasesPage : Int
    , showCurrentSpinner : Bool
    , closingCases : List CaseInfo
    , closingCasesPage : Int
    , showClosingSpinner : Bool
    , navbarState : Navbar.State
    , nowMillis : Int
    }


init : PageContext -> Params.Cases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { currentCases = []
      , currentCasesPage = 1
      , showCurrentSpinner = True
      , closingCases = []
      , closingCasesPage = 1
      , showClosingSpinner = True
      , navbarState = navbarState
      , nowMillis = 0
      }
    , navbarCmd
    , Cmd.none
    )



-- UPDATE


type CasesType
    = Current
    | Closing


type Msg
    = CurrentCase Int
    | ClosingCase Int
    | SearchCases
    | GetCurrentCases (Result Http.Error (List CaseInfo))
    | GetClosingCases (Result Http.Error (List CaseInfo))
    | CasesPrevPage CasesType
    | CasesNextPage CasesType
    | NavbarMsg Navbar.State
    | UpdateCases
    | NowTime Time.Posix


update : PageContext -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update context msg model =
    case msg of
        CurrentCase caseId ->
            ( model
            , Ports.log <| "CurrentCase " ++ String.fromInt caseId
            , Spa.Page.send <| Global.ShowCase caseId
            )

        ClosingCase caseId ->
            ( model
            , Ports.log <| "ClosingCase " ++ String.fromInt caseId
            , Spa.Page.send <| Global.ShowCase caseId
            )

        SearchCases ->
            ( model
            , Cmd.none
            , Spa.Page.send Global.SearchCases
            )

        GetCurrentCases result ->
            case result of
                Err e ->
                    ( { model | showCurrentSpinner = False }
                    , Ports.log <| "Error get current latest cases"
                    , Cmd.none
                    )

                Ok currentCases ->
                    ( { model
                        | currentCases = currentCases
                        , currentCasesPage = 1
                        , showCurrentSpinner = False
                        , closingCases = []
                        , closingCasesPage = 1
                        , showClosingSpinner = True
                      }
                    , Api.getLatestClosingCases GetClosingCases
                    , Cmd.none
                    )

        GetClosingCases result ->
            case result of
                Err _ ->
                    ( { model | showClosingSpinner = False }
                    , Ports.log "Error get closing latest cases"
                    , Cmd.none
                    )

                Ok closingCases ->
                    ( { model
                        | closingCases = closingCases
                        , closingCasesPage = 1
                        , showClosingSpinner = False
                      }
                    , Task.perform NowTime Time.now
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

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Api.getLatestCurrentCases GetCurrentCases
            , Cmd.none
            )

        UpdateCases ->
            ( { model
                | currentCases = []
                , currentCasesPage = 1
                , showCurrentSpinner = True
              }
            , Api.getLatestCurrentCases GetCurrentCases
            , Cmd.none
            )

        NowTime newTime ->
            ( { model
                | nowMillis = Time.posixToMillis newTime
              }
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
        [ ( True, NavbarMsg model.navbarState, "Текущие заявки" )
        , ( False, SearchCases, "Поиск заявок" )
        ]
    <|
        div []
            [ viewCurrentCases model
            , div [] []
            , viewClosingCases model
            ]


viewSpinner =
    Spinner.spinner
        [ Spinner.attrs
            [ Attrs.style "width" "10rem"
            , Attrs.style "height" "10rem"
            ]
        ]
        []


viewCasesTitle : String -> String -> CasesType -> Html Msg
viewCasesTitle title pageNumber caseType =
    Grid.row [ Row.attrs [ Spacing.p1 ] ]
        [ Grid.col [ Col.sm1, Col.middleSm ] [ text title ]
        , Grid.col [ Col.sm1, Col.middleSm ]
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick UpdateCases ]
                ]
                [ Icon.syncAlt
                    |> Icon.present
                    |> Icon.styled [ Icon.xs ]
                    |> Icon.view
                ]
            ]
        , Grid.col [ Col.xs1, Col.attrs [ Spacing.p1 ] ]
            [ ButtonGroup.buttonGroup []
                [ ButtonGroup.button
                    [ Button.primary
                    , Button.attrs [ onClick <| CasesPrevPage caseType ]
                    ]
                    [ text "<" ]
                , ButtonGroup.button
                    [ Button.primary
                    , Button.attrs [ onClick <| CasesNextPage caseType ]
                    ]
                    [ text ">" ]
                ]
            ]
        , Grid.col [ Col.sm9, Col.middleSm, Col.attrs [ Spacing.p0 ] ] [ text pageNumber ]
        ]


casesTableAttrs =
    [ Table.hover
    , Table.bordered
    , Table.small
    , Table.responsiveLg
    , Table.attr (Attrs.style "background-color" Ui.colors.casesBg)
    ]


thA =
    [ Table.cellAttr <| Attrs.attribute "class" "text-center" ]


viewCurrentCases : Model -> Html Msg
viewCurrentCases model =
    let
        remain : Maybe ISO8601.Time -> ( List (Table.CellOption msg), String )
        remain accordTime =
            case accordTime of
                Nothing ->
                    ( [], "" )

                Just accordTime_ ->
                    if model.nowMillis == 0 then
                        ( [], "" )

                    else
                        let
                            diff =
                                ISO8601.diff accordTime_
                                    (ISO8601.fromTime model.nowMillis)

                            opts =
                                []
                        in
                        if diff < 0 then
                            ( [ Table.cellDanger ], "Опоздание" )

                        else if
                            diff < 5 * 60 * 1000
                            -- 5 minutes
                        then
                            ( [ Table.cellWarning ], "< 5 min" )

                        else
                            ( [ Table.cellSuccess ], "> 5 min" )
    in
    Grid.row [] <|
        if model.showCurrentSpinner then
            [ Grid.col [ Col.textAlign Text.alignXsCenter ]
                [ viewSpinner ]
            ]

        else
            [ Grid.col []
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
                , Table.table
                    { options = casesTableAttrs
                    , thead =
                        Table.simpleThead
                            [ Table.th thA [ text "Заявка" ]
                            , Table.th thA [ text "Дата" ]
                            , Table.th thA [ text "Тип услуги" ]
                            , Table.th thA [ text "Статус" ]
                            , Table.th thA [ text "ОВНОУ (п)" ]
                            , Table.th thA [ text "Остаток времени (таймер)" ]
                            , Table.th thA [ text "Изменить статус" ]
                            , Table.th thA [ text "Марка/Модель" ]
                            , Table.th thA [ text "Адрес места поломки" ]
                            , Table.th thA [ text "Тип оплаты" ]
                            ]
                    , tbody =
                        Table.tbody [] <|
                            List.map (Table.tr []) <|
                                List.map
                                    (\theCase ->
                                        let
                                            ( opts, label ) =
                                                remain theCase.accordTime
                                        in
                                        [ Table.td
                                            [ Table.cellAttr Spacing.m0
                                            , Table.cellAttr (onClick <| CurrentCase theCase.id)
                                            ]
                                            [ Ui.idCell CurrentCase theCase.id theCase.services ]
                                        , Table.td [] [ Ui.timeCell theCase.callDate ]
                                        , Table.td [] [ Ui.cell theCase.typeOfService ]
                                        , Table.td [] [ Ui.cell theCase.status ]
                                        , Table.td [] [ Ui.timeCell theCase.accordTime ]
                                        , Table.td opts [ Ui.centerCell label ]
                                        , Table.td [] [ Ui.cell theCase.status ]
                                        , Table.td [] [ Ui.cell theCase.makeModel ]
                                        , Table.td [] [ Ui.addressCell theCase.breakdownPlace ]
                                        , Table.td [] [ Ui.cell theCase.payType ]
                                        ]
                                    )
                                    (model.currentCases
                                        |> List.drop ((model.currentCasesPage - 1) * pageSize)
                                        |> List.take pageSize
                                    )
                    }
                ]
            ]


viewClosingCases : Model -> Html Msg
viewClosingCases model =
    Grid.row [] <|
        if model.showClosingSpinner then
            [ Grid.col [ Col.textAlign Text.alignXsCenter ] <|
                [ viewSpinner ]
            ]

        else
            [ Grid.col []
                [ viewCasesTitle "Закрытие заявок"
                    (String.fromInt model.closingCasesPage
                        ++ " / "
                        ++ String.fromInt (ceiling (toFloat (List.length model.closingCases) / toFloat pageSize))
                    )
                    Closing
                , Table.table
                    { options = casesTableAttrs
                    , thead =
                        Table.simpleThead
                            [ Table.th thA [ text "Заявка" ]
                            , Table.th thA [ text "Дата" ]
                            , Table.th thA [ text "Тип услуги" ]
                            , Table.th thA [ text "Статус" ]
                            , Table.th thA [ text "ОВНОУ (п)" ]
                            , Table.th thA [ text "Изменить статус" ]
                            , Table.th thA [ text "Марка/Модель" ]
                            , Table.th thA [ text "Адрес места поломки" ]
                            , Table.th thA [ text "Тип оплаты" ]
                            ]
                    , tbody =
                        Table.tbody [] <|
                            List.map (Table.tr []) <|
                                List.map
                                    (\theCase ->
                                        [ Table.td [] [ Ui.idCell CurrentCase theCase.id theCase.services ]
                                        , Table.td [] [ Ui.timeCell theCase.callDate ]
                                        , Table.td [] [ Ui.cell theCase.typeOfService ]
                                        , Table.td [] [ Ui.cell theCase.status ]
                                        , Table.td [] [ Ui.timeCell theCase.accordTime ]
                                        , Table.td [] [ Ui.cell theCase.status ]
                                        , Table.td [] [ Ui.cell theCase.makeModel ]
                                        , Table.td [] [ Ui.addressCell theCase.breakdownPlace ]
                                        , Table.td [] [ Ui.cell theCase.payType ]
                                        ]
                                    )
                                    (model.closingCases
                                        |> List.drop ((model.closingCasesPage - 1) * pageSize)
                                        |> List.take pageSize
                                    )
                    }
                ]
            ]
