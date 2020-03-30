module Pages.ShowCase exposing (Model, Msg, page)

import Api
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing
import Debug
import Generated.Params as Params
import Global
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)
import Http
import Ports
import Spa.Page
import Types exposing (CaseDescription, CaseInfo)
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
    { cases : List CaseInfo
    , caseDescription : CaseDescription
    , closing1 : String
    , closing2 : String
    , closing3 : String
    , closing4 : String
    , inputComment : String
    , commentFileName : String
    , comments : List Comment
    , caseStatus : Maybe CaseStatus
    , navbarState : Navbar.State
    }


init : PageContext -> Params.ShowCase -> ( Model, Cmd Msg, Cmd Global.Msg )
init context params =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { cases = []
      , caseDescription =
            { caseId = 0
            , services = 0
            , serviceType = ""
            , client = ""
            , clientPhone = ""
            , firstAddress = ""
            , lastAddress = ""
            , expectedServiceStart = ""
            , factServiceStart = ""
            , factServiceEnd = ""
            , makeModel = ""
            , plateNumber = ""
            , loadingDifficulty = ""
            , suburbanMilage = ""
            }
      , closing1 = ""
      , closing2 = ""
      , closing3 = ""
      , closing4 = ""
      , inputComment = ""
      , commentFileName = ""
      , comments = []
      , caseStatus = Nothing
      , navbarState = navbarState
      }
    , navbarCmd
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
    | CaseDescriptionArrived (Result Http.Error CaseDescription)
    | NavbarMsg Navbar.State


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

        CaseDescriptionArrived result ->
            case result of
                Err e ->
                    ( model
                    , Ports.log <|
                        "Error get case "
                            ++ String.fromInt context.global.caseId
                    , Cmd.none
                    )

                Ok caseDescription ->
                    ( { model
                        | caseDescription = caseDescription
                      }
                    , Cmd.none
                    , Cmd.none
                    )

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Api.getCase context.global.caseId CaseDescriptionArrived
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



-- VIEW


view : PageContext -> Model -> Html Msg
view context model =
    Ui.page NavbarMsg
        model.navbarState
        context.global.username
        [ ( False, Cases, "Текущие заявки" )
        , ( False, SearchCases, "Поиск заявок" )
        ]
    <|
        div []
            [ viewCasePanel model
            ]


nameStyle =
    [ Attrs.class "name" ]


valueStyle =
    [ Attrs.class "value" ]


name t =
    div nameStyle [ text t ]


value t =
    div valueStyle [ text t ]


field n v =
    Grid.row [ Row.attrs [ Spacing.p2 ] ]
        [ Grid.col [ Col.sm4 ]
            [ name <| n ++ ": " ]
        , Grid.col [ Col.sm8 ]
            [ value v ]
        ]


viewCasePanel : Model -> Html Msg
viewCasePanel model =
    viewCaseVerbose model


viewCaseVerbose : Model -> Html msg
viewCaseVerbose model =
    let
        c =
            model.caseDescription

        caseId =
            String.fromInt c.caseId
                ++ (if c.services > 1 then
                        " / " ++ String.fromInt c.services

                    else
                        ""
                   )
    in
    Grid.row [ Row.attrs [ Spacing.p5 ] ]
        [ Grid.col [] []
        , Grid.col [ Col.attrs [ Attrs.style "background-color" Ui.colors.casesBg ], Col.sm5 ]
            [ field "Номер заявки" caseId
            , field "Вид помощи" c.serviceType
            , field "Клиент" c.client
            , field "Телефон клиента" c.clientPhone
            , field "Адрес начала работы" c.firstAddress
            , field "Адрес окончания работы" ""
            , field "Желаемая дата оказания услуг" c.expectedServiceStart
            , field "Факт. время оказания услуг" c.factServiceStart
            , field "Время окончания работы" c.factServiceEnd
            ]
        , Grid.col [ Col.attrs [ Attrs.style "background-color" Ui.colors.casesBg ], Col.sm5 ]
            [ field "Марка и модель авто" c.makeModel
            , field "Гос. номер" c.plateNumber
            , field "Сложность погрузки" c.loadingDifficulty
            , field "Перепробег за МКАД" c.suburbanMilage
            , field "Простой" ""
            , field "Переадресация" ""
            , field "Стоимость услуги" ""
            , field "KPI" ""
            ]
        , Grid.col [] []
        ]
