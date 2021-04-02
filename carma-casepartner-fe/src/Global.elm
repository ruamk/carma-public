module Global exposing
    ( Flags
    , Model
    , Msg
    , init
    , logout
    , navigate
    , saveUsername
    , serviceId
    , settings
    , instruction
    , subscriptions
    , update
    , view
    )

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Components
import Generated.Route as Route exposing (Route)
import Json.Encode as JE
import Ports
import Task
import Url exposing (Url)



-- INIT


type alias Flags =
    Maybe String


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , username : String
    , serviceId : Int
    , route : String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session : Api.Session
        session =
            case flags of
                Just s ->
                    Api.decodeSession s

                Nothing ->
                    Api.Session "" 0 ""
    in
    ( Model
        flags
        url
        key
        session.username
        session.serviceId
        session.route
    , Cmd.none
    )



-- UPDATE


type Msg
    = Navigate Route
    | Username String
    | ServiceId Int
    | Settings
    | Instruction
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( { model | route = Route.toHref route }
            , Cmd.batch
                [ saveSessions
                    { username = model.username
                    , serviceId = model.serviceId
                    , route = Route.toHref route
                    }
                , Nav.pushUrl model.key (Route.toHref route)
                ]
            )

        Username name ->
            ( { model | username = name }
            , saveSessions
                { username = name
                , serviceId = model.serviceId
                , route = model.route
                }
            )

        ServiceId id ->
            ( { model | serviceId = id }
            , saveSessions
                { username = model.username
                , serviceId = id
                , route = model.route
                }
            )

        Settings ->
            ( model
            , navigate Route.Settings
            )
        
        Instruction ->
            ( model
            , Nav.load "/book/index.html"
            )

        Logout ->
            ( model
            , navigate Route.Login
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    { page : Document msg
    , global : Model
    , toMsg : Msg -> msg
    }
    -> Document msg
view { page, global, toMsg } =
    Components.layout
        { page = page
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)


saveUsername : String -> Cmd Msg
saveUsername username =
    send (Username username)


serviceId : Int -> Cmd Msg
serviceId id =
    send (ServiceId id)


settings : Cmd Msg
settings =
    send Settings

instruction : Cmd Msg
instruction = 
    send Instruction


logout : Cmd Msg
logout =
    send Logout


{-| Save username and serviceId
-}
saveSessions : { username : String, serviceId : Int, route : String } -> Cmd msg
saveSessions session =
    JE.object
        [ ( "username", JE.string session.username )
        , ( "serviceId", JE.int session.serviceId )
        , ( "route", JE.string session.route )
        ]
        |> JE.encode 0
        |> Ports.storeSession
