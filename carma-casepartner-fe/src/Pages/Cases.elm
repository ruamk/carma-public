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
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Generated.Params as Params
import Global
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Http
import Ports
import Spa.Page
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
    , closingCases : List CaseInfo
    , closingCasesPage : Int
    , navbarState : Navbar.State
    }


init : PageContext -> Params.Cases -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { currentCases = []
      , currentCasesPage = 1
      , closingCases = []
      , closingCasesPage = 1
      , navbarState = navbarState
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
                    ( model
                    , Ports.log <| "Error get current latest cases" ++ Debug.toString e
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

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Api.getLatestCurrentCases GetCurrentCases
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


viewCasesTitle : String -> String -> CasesType -> Html Msg
viewCasesTitle title pageNumber caseType =
    Grid.row [ Row.attrs [ Spacing.p1 ] ]
        [ Grid.col [ Col.sm1, Col.middleSm ] [ text title ]
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
        , Grid.col [ Col.sm10, Col.middleSm, Col.attrs [ Spacing.p0 ] ] [ text pageNumber ]
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
    div []
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
                                [ Table.td
                                    [ Table.cellAttr Spacing.m0
                                    , Table.cellAttr (onClick <| CurrentCase theCase.id)
                                    ]
                                    [ Ui.idCell CurrentCase theCase.id theCase.services ]
                                , Table.td [] [ Ui.cell theCase.callDate ]
                                , Table.td [] [ Ui.cell theCase.typeOfService ]
                                , Table.td [] [ Ui.cell theCase.status ]
                                , Table.td [] [ Ui.cell theCase.accordTime ]
                                , Table.td [] [ Ui.cell theCase.remainTime ]
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


viewClosingCases : Model -> Html Msg
viewClosingCases model =
    div []
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
                                , Table.td [] [ Ui.cell theCase.callDate ]
                                , Table.td [] [ Ui.cell theCase.typeOfService ]
                                , Table.td [] [ Ui.cell theCase.status ]
                                , Table.td [] [ Ui.cell theCase.accordTime ]
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
