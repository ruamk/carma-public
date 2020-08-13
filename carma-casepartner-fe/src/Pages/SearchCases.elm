{-
   страничка : "Поиск заявок"

   Полностью копируем табличку Закрытие заявок на странице "Текущие заявки"

   Только убираем выборку по статусам, т.е. выводятся вообще все кейсы с пагинацией

   можно по 25 строк на страницу
-}


module Pages.SearchCases exposing (Flags, Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Generated.Route as Route
import Global
import Html
    exposing
        ( Html
        , b
        , br
        , div
        , h2
        , h3
        , hr
        , li
        , p
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( action
        , attribute
        , class
        , placeholder
        , style
        , value
        )
import Html.Events exposing (onClick)
import Http
import MessageToast exposing (MessageToast)
import Page exposing (Document, Page)
import Types exposing (ServiceInfo)
import Ui


pageSize : Int
pageSize =
    20


maxServices : Int
maxServices =
    pageSize * 100


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
    }


type Msg
    = NavbarMsg Navbar.State
    | Logout
    | UsermenuMsg Dropdown.State
    | Services
    | Service Int
    | UpdateCustomMessageToast (MessageToast Msg)
    | ServicesDownloaded (Result Http.Error (List ServiceInfo))
    | ServicesFirstPage
    | ServicesPrevPage
    | ServicesNextPage


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
            , Api.getServices ((model.servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

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

        ServicesFirstPage ->
            let
                servicesPage =
                    1
            in
            ( { model | servicesPage = servicesPage }
            , Api.getServices ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

        ServicesPrevPage ->
            let
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
            , Api.getServices ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )

        ServicesNextPage ->
            let
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
            , Api.getServices ((servicesPage - 1) * pageSize)
                pageSize
                ServicesDownloaded
            , Cmd.none
            )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
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
            , usermenuMsg = UsermenuMsg
            , navbarState = model.navbarState
            , usermenuState = model.usermenuState
            , username = global.username
            , buttons =
                [ ( False, Services, "Текущие заявки" )
                , ( True, NavbarMsg model.navbarState, "Поиск заявок" )
                ]
            }
          <|
            div []
                [ viewServices model
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


hC =
    Table.cellAttr <| class "text-center"


vC =
    Table.cellAttr <| class "align-middle"


hideMobile =
    Table.cellAttr <| class "d-none d-md-table-cell"


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
                    [ Button.primary ]
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
            [ viewServicesTitle model
                "Все заявки"
                (String.fromInt model.servicesPage)
            , Table.table
                { options = servicesTableAttrs
                , thead =
                    let
                        ha =
                            [ hC, vC ]

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
                                    , Table.td [ hC, vC, thW 10 ] [ Ui.cell theCase.typeOfService ]
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
            ]
        ]
