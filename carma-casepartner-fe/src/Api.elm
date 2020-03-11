module Api exposing
    ( getLatestClosingCases
    , getLatestCurrentCases
    , login
    )

import Http
import HttpBuilder
import Json.Decode exposing (Decoder, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import Types exposing (TheCase)
import Url.Builder as UB


type Msg
    = Login
    | Logout


apiLogin : String
apiLogin =
    "/api/v1/login"


apiLogout : String
apiLogout =
    "/api/v1/logout"


apiGetLatestCurrentCases : String
apiGetLatestCurrentCases =
    "/api/v1/getLatestCases/current"


apiGetLatestClosingCases : String
apiGetLatestClosingCases =
    "/api/v1/getLatestCases/closing"


login : String -> String -> (Result Http.Error Int -> msg) -> Cmd msg
login name password message =
    let
        body =
            [ ( "login", name )
            , ( "password", password )
            ]
    in
    HttpBuilder.post apiLogin
        |> HttpBuilder.withUrlEncodedBody body
        |> HttpBuilder.withExpect (loginExpect message)
        |> HttpBuilder.request


loginExpect : (Result Http.Error Int -> msg) -> Http.Expect msg
loginExpect toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Ok metadata.statusCode

                Http.GoodStatus_ metadata body ->
                    Ok metadata.statusCode


latestCasesDecoder : Decoder (List TheCase)
latestCasesDecoder =
    list decodeTheCase


decodeTheCase : Decoder TheCase
decodeTheCase =
    succeed TheCase
        |> required "id" string
        |> optional "callDate" string "default callDate"
        |> optional "typeOfService" string "default typeOfService"
        |> optional "status" string "default status"
        |> optional "accordTime" string "default accordTime"
        |> optional "remainTime" string "01:00"
        |> optional "makeModel" string "default makeModel"
        |> optional "breakdownPlace" string "default breakdownPlace"
        |> optional "payType" string "default payType"


getLatestCurrentCases : (Result Http.Error (List TheCase) -> msg) -> Cmd msg
getLatestCurrentCases message =
    HttpBuilder.post apiGetLatestCurrentCases
        |> HttpBuilder.withExpect (Http.expectJson message latestCasesDecoder)
        |> HttpBuilder.request


getLatestClosingCases : (Result Http.Error (List TheCase) -> msg) -> Cmd msg
getLatestClosingCases message =
    HttpBuilder.post apiGetLatestClosingCases
        |> HttpBuilder.withExpect (Http.expectJson message latestCasesDecoder)
        |> HttpBuilder.request
