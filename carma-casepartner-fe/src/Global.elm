module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route, routes)
import Ports


type alias Flags =
    ()


type alias Model =
    { username : String
    , caseId : String
    }


type Msg
    = Login
    | Logout
    | Cases String
    | SearchCases
    | ShowCase String


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init commands _ =
    ( { username = ""
      , caseId = ""
      }
    , Cmd.none
      -- , commands.navigate routes.login
    , commands.navigate routes.cases
    )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update commands msg model =
    case msg of
        Cases username ->
            ( { model | username = username }
            , Cmd.none
            , commands.navigate routes.cases
            )

        SearchCases ->
            ( model
            , Cmd.none
            , commands.navigate routes.searchCases
            )

        ShowCase caseId ->
            ( { model | caseId = caseId }
            , Cmd.none
            , commands.navigate routes.showCase
            )

        -- todo: remove
        _ ->
            ( model
            , Cmd.none
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
