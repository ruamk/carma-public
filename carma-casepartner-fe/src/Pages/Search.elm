{-
   страничка : "Поиск заявок"

   Полностью копируем табличку Закрытие заявок на странице "Текущие заявки"

   Только убираем выборку по статусам, т.е. выводятся вообще все кейсы с пагинацией

   можно по 25 строк на страницу
-}


module Pages.Search exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Dict
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , br
        , div
        , h3
        , h4
        , text
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , style
        )
import Html.Events exposing (onClick)
import Http
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Types exposing (Dictionary, ServiceInfo)
import Ui


pageSize : Int
pageSize =
    20


spinnerSize : String
spinnerSize =
    "0.7rem"


type alias Flags =
    ()


type alias Model =
    { navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , messageToast : MessageToast Msg
    , showSpinner : Bool
    , services : List ServiceInfo
    , servicesPage : Int
    , serviceId : String
    , startDate : String
    , endDate : String
    , searchCondition : Api.SearchCondition
    , typeOfServiceSynonym : Dictionary
    }


type Msg
    = NavbarMsg Navbar.State
    | ClearServiceId
    | ClearStartDate
    | ClearEndDate
    | InputServiceId String
    | InputStartDate String
    | InputEndDate String
    | Logout
    | Search
    | Services
    | Service Int
    | ServicesDownloaded (Result Http.Error (List ServiceInfo))
    | ServicesFirstPage
    | ServicesPrevPage
    | ServicesNextPage
    | Settings
    | TypeOfServiceSynonymDownloaded (Result Http.Error Dictionary)
    | UpdateCustomMessageToast (MessageToast Msg)
    | UsermenuMsg Dropdown.State


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ _ =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState
      , usermenuState = Dropdown.initialState
      , messageToast =
            MessageToast.initWithConfig UpdateCustomMessageToast
                { delayInMs = 2000
                , toastsToShow = 10
                }
      , showSpinner = True
      , services = []
      , servicesPage = 1
      , serviceId = ""
      , startDate = ""
      , endDate = ""
      , searchCondition = Api.SearchAll
      , typeOfServiceSynonym = Dict.empty
      }
    , navbarCmd
    , Cmd.none
    )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update _ msg model =
    case msg of
        -- Entry point
        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.batch
                [ Api.getTypeOfServiceSynonym TypeOfServiceSynonymDownloaded
                , Api.getServices model.searchCondition
                    ((model.servicesPage - 1) * pageSize)
                    pageSize
                    ServicesDownloaded
                ]
            , Cmd.none
            )

        ClearServiceId ->
            ( { model | serviceId = "" }
            , Cmd.none
            , Cmd.none
            )

        ClearStartDate ->
            ( { model | startDate = "" }, Cmd.none, Cmd.none )

        ClearEndDate ->
            ( { model | endDate = "" }, Cmd.none, Cmd.none )

        InputServiceId s ->
            -- Only positive integer value is valid
            let
                serviceId : Maybe Int
                serviceId =
                    String.toInt s
            in
            ( { model
                | serviceId =
                    case serviceId of
                        Just i ->
                            if i > 0 then
                                s

                            else
                                model.serviceId

                        _ ->
                            if String.isEmpty s then
                                s

                            else
                                model.serviceId
                , startDate = ""
                , endDate = ""
              }
            , Cmd.none
            , Cmd.none
            )

        InputStartDate s ->
            ( { model | startDate = s }, Cmd.none, Cmd.none )

        InputEndDate s ->
            ( { model | endDate = s }, Cmd.none, Cmd.none )

        Logout ->
            ( model
            , Cmd.none
            , Global.logout
            )

        UsermenuMsg state ->
            ( { model | usermenuState = state }
            , Cmd.none
            , Cmd.none
            )

        Search ->
            let
                searchCondition : Api.SearchCondition
                searchCondition =
                    if String.isEmpty model.serviceId then
                        case ( model.startDate, model.endDate ) of
                            ( "", "" ) ->
                                Api.SearchAll

                            _ ->
                                Api.SearchCallDate
                                    ( model.startDate
                                    , model.endDate
                                    )

                    else
                        Api.SearchServiceId model.serviceId
            in
            ( { model
                | servicesPage = 1
                , searchCondition = searchCondition
              }
            , Api.getServices
                searchCondition
                0
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

        Services ->
            ( model
            , Cmd.none
            , Global.navigate Route.Services
            )

        Service serviceId ->
            ( model
            , Cmd.none
            , Cmd.batch
                [ Global.serviceId serviceId
                , Global.navigate Route.ShowService
                ]
            )

        UpdateCustomMessageToast updatedMessageToast ->
            ( { model | messageToast = updatedMessageToast }
            , Cmd.none
            , Cmd.none
            )

        ServicesDownloaded result ->
            case result of
                Err _ ->
                    let
                        messageToast : MessageToast Msg
                        messageToast =
                            model.messageToast
                                |> MessageToast.danger
                                |> MessageToast.withMessage "Error get services"
                    in
                    ( { model
                        | showSpinner = False
                        , messageToast = messageToast
                      }
                    , Cmd.none
                    , Cmd.none
                    )

                Ok services ->
                    ( { model
                        | showSpinner = False
                        , services = services
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        Settings ->
            ( model
            , Cmd.none
            , Global.settings
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

        ServicesFirstPage ->
            let
                servicesPage : Int
                servicesPage =
                    1
            in
            ( { model | servicesPage = servicesPage }
            , Api.getServices
                model.searchCondition
                ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

        ServicesPrevPage ->
            let
                servicesPage : Int
                servicesPage =
                    if model.servicesPage > 1 then
                        model.servicesPage - 1

                    else
                        1
            in
            ( { model
                | servicesPage = servicesPage
                , showSpinner = True
              }
            , Api.getServices
                model.searchCondition
                ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

        ServicesNextPage ->
            let
                servicesPage : Int
                servicesPage =
                    if List.length model.services == pageSize then
                        model.servicesPage + 1

                    else
                        model.servicesPage
            in
            ( { model
                | servicesPage = servicesPage
                , showSpinner = True
              }
            , Api.getServices
                model.searchCondition
                ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Dropdown.subscriptions model.usermenuState UsermenuMsg
        ]


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Поиск заявок"
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
                [ ( False, Services, "Текущие заявки" )
                , ( True, NavbarMsg model.navbarState, "Поиск заявок" )
                , ( False, Settings, "Настройки" )
                ]
            }
          <|
            div []
                [ Grid.row []
                    [ Grid.col [ Col.sm2 ] [ viewSearchPanel model ]
                    , Grid.col [ Col.sm10 ] [ viewServices model ]
                    ]
                , br [] []
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


hC : Table.CellOption msg
hC =
    Table.cellAttr <| class "text-center"


vC : Table.CellOption msg
vC =
    Table.cellAttr <| class "align-middle"


hideMobile : Table.CellOption msg
hideMobile =
    Table.cellAttr <| class "d-none d-md-table-cell"


thW : Int -> Table.CellOption msg
thW w =
    Table.cellAttr <| attribute "width" (String.fromInt w ++ "%")


colorOfPay : String -> List (Table.CellOption msg)
colorOfPay payType =
    case payType of
        "Клиент" ->
            [ cellAttrWarning ]

        "Смешанный" ->
            [ cellAttrWarning ]

        _ ->
            []


cellAttrWarning : Table.CellOption msg
cellAttrWarning =
    Table.cellAttr <| class "table-warning"


servicesTableAttrs : List (Table.TableOption msg)
servicesTableAttrs =
    [ Table.bordered
    , Table.striped
    , Table.small
    , Table.responsiveLg
    , Table.attr (style "background-color" Ui.colors.casesBg)
    ]


viewServicesTitle : Model -> String -> String -> Html Msg
viewServicesTitle model title pageNumber =
    Grid.row [ Row.attrs [ Spacing.p1, Flex.row ] ]
        [ Grid.col
            [ Col.md6
            , Col.orderMd1
            , Col.orderLg3
            , Col.attrs [ Flex.alignSelfCenter ]
            ]
            [ h3 [ style "margin" "0 0 0 0" ] [ text title ] ]
        , Grid.col
            [ Col.sm2
            , Col.attrs [ Spacing.p1, Spacing.pl3 ]
            ]
            [ ButtonGroup.buttonGroup []
                [ ButtonGroup.button
                    [ Button.primary
                    , Button.disabled (model.servicesPage == 1)
                    , Button.attrs [ onClick ServicesFirstPage ]
                    ]
                    [ text "|<" ]
                , ButtonGroup.button
                    [ Button.primary
                    , Button.disabled (model.servicesPage == 1)
                    , Button.attrs [ onClick ServicesPrevPage ]
                    ]
                    [ text "<" ]
                , ButtonGroup.button
                    [ Button.primary
                    , Button.attrs
                        [ style "width" "50px"
                        ]
                    ]
                    [ if model.showSpinner then
                        Ui.viewSpinner spinnerSize

                      else
                        text pageNumber
                    ]
                , ButtonGroup.button
                    [ Button.primary
                    , Button.disabled (List.length model.services < pageSize)
                    , Button.attrs [ onClick ServicesNextPage ]
                    ]
                    [ text ">" ]
                ]
            ]
        ]


viewServices : Model -> Html Msg
viewServices model =
    Grid.row []
        [ Grid.col []
            [ Table.table
                { options = servicesTableAttrs
                , thead =
                    let
                        ha : List (Table.CellOption msg)
                        ha =
                            [ hC, vC ]

                        th : String -> Table.Cell Msg
                        th t =
                            Table.th ha [ text t ]
                    in
                    Table.simpleThead
                        [ th "Заявка"
                        , th "Дата подачи"
                        , th "Услуга"
                        , th "Марка/Модель"
                        , Table.th (hideMobile :: ha) [ text "Адрес места поломки" ]
                        , th "Тип оплаты"
                        ]
                , tbody =
                    Table.tbody [] <|
                        List.map (Table.tr []) <|
                            List.map
                                (\theCase ->
                                    [ Table.td [ hC, vC, thW 3 ]
                                        [ Ui.idCell Service
                                            theCase.serviceId
                                            theCase.caseId
                                            theCase.serviceSerial
                                        ]
                                    , Table.td [ hC, vC, thW 5 ] [ Ui.dateCell theCase.callDate ]
                                    , Table.td [ hC, vC, thW 10 ]
                                        [ Ui.cell <|
                                            case theCase.typeOfService of
                                                Just tos ->
                                                    case Dict.get tos model.typeOfServiceSynonym of
                                                        Just v ->
                                                            v

                                                        Nothing ->
                                                            tos

                                                Nothing ->
                                                    ""
                                        ]
                                    , Table.td [ hC, vC, thW 15 ] [ Ui.cell theCase.makeModel ]
                                    , Table.td [ hideMobile, vC ] [ Ui.addressCell theCase.breakdownPlace ]
                                    , Table.td
                                        (colorOfPay theCase.payType
                                            ++ [ hC, vC, thW 10 ]
                                        )
                                        [ Ui.cell theCase.payType ]
                                    ]
                                )
                                model.services
                }
            , viewServicesTitle model
                ""
                (String.fromInt model.servicesPage)
            ]
        ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    div
        [ style "padding-left" "10px"
        , style "padding-top" "20px"
        , style "padding-bottom" "20px"
        ]
        [ Grid.row [ Row.attrs [ Spacing.pt3, Flex.row ] ]
            [ Grid.col [ Col.attrs [ Flex.alignSelfCenter ] ]
                [ h4 [] [ text "Номер заявки:" ] ]
            ]
        , Grid.row [ Row.attrs [ Flex.row ] ]
            [ Grid.col [ Col.attrs [ Flex.alignSelfCenter ] ]
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.value model.serviceId
                        , Input.onInput InputServiceId
                        ]
                    )
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.outlineSecondary
                            , Button.attrs
                                [ onClick ClearServiceId
                                , Spacing.pl3
                                , Spacing.pr3
                                , style "border-color" "#ced4da"
                                ]
                            ]
                            [ Icon.times
                                |> Icon.present
                                |> Icon.styled [ Icon.sm ]
                                |> Icon.view
                            ]
                        ]
                    |> InputGroup.view
                ]
            ]
        , Grid.row [ Row.attrs [ Spacing.pt3, Flex.row ] ]
            [ Grid.col [ Col.attrs [ Flex.alignSelfCenter ] ]
                [ h4 [] [ text "Дата подачи:" ] ]
            ]
        , Grid.row [ Row.attrs [ Flex.row ] ]
            [ Grid.col [ Col.attrs [ Flex.alignSelfCenter ] ]
                [ InputGroup.config
                    (InputGroup.date
                        [ Input.onInput InputStartDate
                        , Input.value model.startDate
                        ]
                    )
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.outlineSecondary
                            , Button.attrs
                                [ onClick ClearStartDate
                                , Spacing.pl3
                                , Spacing.pr3
                                , style "border-color" "#ced4da"
                                ]
                            ]
                            [ Icon.times
                                |> Icon.present
                                |> Icon.styled [ Icon.sm ]
                                |> Icon.view
                            ]
                        ]
                    |> InputGroup.view
                ]
            ]
        , Grid.row [ Row.attrs [ Spacing.pt1, Flex.row ] ]
            [ Grid.col [ Col.attrs [ Flex.alignSelfCenter ] ]
                [ InputGroup.config
                    (InputGroup.date
                        [ Input.onInput InputEndDate
                        , Input.value model.endDate
                        ]
                    )
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.outlineSecondary
                            , Button.attrs
                                [ onClick ClearEndDate
                                , Spacing.pl3
                                , Spacing.pr3
                                , style "border-color" "#ced4da"
                                ]
                            ]
                            [ Icon.times
                                |> Icon.present
                                |> Icon.styled [ Icon.sm ]
                                |> Icon.view
                            ]
                        ]
                    |> InputGroup.view
                ]
            ]
        , Grid.row [ Row.attrs [ Spacing.pt3, Flex.row ] ]
            [ Grid.col
                [ Col.sm12
                , Col.attrs [ Flex.alignSelfCenter ]
                , Col.textAlign Text.alignXsCenter
                ]
                [ Button.button
                    [ Button.primary
                    , Button.large
                    , Button.attrs [ onClick Search ]
                    ]
                    [ text "Искать" ]
                ]
            ]
        ]
