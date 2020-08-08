module Chat exposing
    ( ChatMessage(..)
    , ChatUser
    , caseReceiver
    , connectToCase
    , decodeChat
    )

import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , errorToString
        , field
        , int
        , list
        , map2
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (required)
import Ports
import Url as Url exposing (Url)


type alias ChatUser =
    { ip : String
    , id : Int
    }


type ChatMessage
    = Joined ChatUser
    | YouAreNotAlone (List ChatUser)
    | Left ChatUser
    | Message String ChatUser


connectToCase : Url -> Int -> Cmd msg
connectToCase url caseId =
    let
        protocol =
            case url.protocol of
                Url.Http ->
                    "ws://"

                Url.Https ->
                    "wss://"

        port_ =
            case url.port_ of
                Just p ->
                    ":" ++ String.fromInt p

                Nothing ->
                    ""

        chatUrl =
            protocol
                ++ url.host
                ++ port_
                ++ "/chat/Case:"
                ++ String.fromInt caseId
    in
    Ports.caseChatConnect chatUrl


caseReceiver : (String -> msg) -> Sub msg
caseReceiver f =
    Ports.caseChatMessageReceiver f


decodeChat : String -> Result String ChatMessage
decodeChat value =
    let
        decodeChatUser : Decoder ChatUser
        decodeChatUser =
            map2 ChatUser
                (field "ip" string)
                (field "id" int)

        decodeJoined : Decoder ChatMessage
        decodeJoined =
            succeed (\user -> Joined user)
                |> required "joined" decodeChatUser

        decodeLeft : Decoder ChatMessage
        decodeLeft =
            succeed (\user -> Left user)
                |> required "left" decodeChatUser

        decodeMessage : Decoder ChatMessage
        decodeMessage =
            succeed (\message user -> Message message user)
                |> required "msg" string
                |> required "user" decodeChatUser

        decodeYouAreNotAlone : Decoder ChatMessage
        decodeYouAreNotAlone =
            succeed (\users -> YouAreNotAlone users)
                |> required "youAreNotAlone" (list decodeChatUser)

        decoder : Decoder ChatMessage
        decoder =
            oneOf
                [ decodeJoined
                , decodeLeft
                , decodeMessage
                , decodeYouAreNotAlone
                ]
    in
    case decodeString decoder value of
        Ok v ->
            Ok v

        Err e ->
            Err <| errorToString e
