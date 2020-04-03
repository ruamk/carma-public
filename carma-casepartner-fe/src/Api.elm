module Api exposing
    ( getCase
    , getLatestClosingCases
    , getLatestCurrentCases
    , login
    )

import Http
import HttpBuilder
import ISO8601 exposing (Time)
import Json.Decode exposing (Decoder, andThen, fail, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import Types exposing (CaseDescription, CaseInfo)
import Url.Builder as UB


type Msg
    = Login
    | Logout



-- |  In production should be empty, for elm-live with hot reload


prefix : String
prefix = ""


apiLogin : String
apiLogin =
    prefix ++ "/api/v1/login"


apiLogout : String
apiLogout =
    prefix ++ "/api/v1/logout"


apiGetLatestCurrentCases : String
apiGetLatestCurrentCases =
    prefix ++ "/api/v1/getLatestCases/current"


apiGetLatestClosingCases : String
apiGetLatestClosingCases =
    prefix ++ "/api/v1/getLatestCases/closing"


apiGetCase : String
apiGetCase =
    prefix ++ "/api/v1/getCase/"


login : String -> String -> (Result Http.Error Int -> msg) -> Cmd msg
login name password message =
    let
        postBody =
            [ ( "login", name )
            , ( "password", password )
            ]

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
    in
    HttpBuilder.post apiLogin
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (loginExpect message)
        |> HttpBuilder.request


latestCasesDecoder : Decoder (List CaseInfo)
latestCasesDecoder =
    let
        decodeCaseInfo : Decoder CaseInfo
        decodeCaseInfo =
            succeed CaseInfo
                |> required "caseId" int
                |> optional "services" int 0
                |> optional "callDate" (nullable ISO8601.decode) Nothing
                |> optional "typeOfService" string ""
                |> optional "status" string ""
                |> optional "accordTime" (nullable ISO8601.decode) Nothing
                |> optional "makeModel" string ""
                |> optional "breakdownPlace" string ""
                |> optional "payType" string ""
    in
    list decodeCaseInfo


getLatestCurrentCases : (Result Http.Error (List CaseInfo) -> msg) -> Cmd msg
getLatestCurrentCases message =
    HttpBuilder.post apiGetLatestCurrentCases
        |> HttpBuilder.withExpect (Http.expectJson message latestCasesDecoder)
        |> HttpBuilder.request


getLatestClosingCases : (Result Http.Error (List CaseInfo) -> msg) -> Cmd msg
getLatestClosingCases message =
    HttpBuilder.post apiGetLatestClosingCases
        |> HttpBuilder.withExpect (Http.expectJson message latestCasesDecoder)
        |> HttpBuilder.request


getCase : Int -> (Result Http.Error CaseDescription -> msg) -> Cmd msg
getCase caseId message =
    let
        getCaseDecoder : Decoder CaseDescription
        getCaseDecoder =
            succeed CaseDescription
                |> required "caseId" int
                |> optional "services" int 0
                |> optional "serviceType" string ""
                |> optional "client" string ""
                |> optional "clientPhone" string ""
                |> optional "firstAddress" string ""
                |> optional "lastAddress" string ""
                |> optional "expectedServiceStart" string ""
                |> optional "factServiceStart" string ""
                |> optional "factServiceEnd" string ""
                |> optional "makeModel" string ""
                |> optional "plateNumber" string ""
                |> optional "loadingDifficulty" string ""
                |> optional "suburbanMilage" string ""
    in
    HttpBuilder.get (apiGetCase ++ String.fromInt caseId)
        |> HttpBuilder.withExpect (Http.expectJson message getCaseDecoder)
        |> HttpBuilder.request
