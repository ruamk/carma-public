port module Ports exposing
    ( caseChatConnect
    , caseChatMessageReceiver
    , caseChatSendMessage
    , storeSession
    )


port caseChatConnect : String -> Cmd msg


port caseChatMessageReceiver : (String -> msg) -> Sub msg


port caseChatSendMessage : String -> Cmd msg


port storeSession : String -> Cmd msg
