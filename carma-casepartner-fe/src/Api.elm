module Api exposing
    ( Session
    , decodeSession
    , getCaseComments
    , getLatestClosingCases
    , getLatestCurrentCases
    , getService
    , login
    , postServiceComment
    )

import Http
import HttpBuilder
import ISO8601 exposing (Time)
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , at
        , bool
        , decodeString
        , dict
        , fail
        , field
        , float
        , int
        , list
        , map2
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , ClosingCaseInfo
        , CurrentCaseInfo
        , ServiceDescription
        )
import Url.Builder as UB


type Msg
    = Login
    | Logout


type alias Session =
    { username : String, serviceId : Int }


decodeSession s =
    case decodeString sessionDecoder s of
        Ok session ->
            session

        Err _ ->
            Session "" 0


sessionDecoder : Decoder Session
sessionDecoder =
    map2 Session
        (at [ "username" ] string)
        (at [ "serviceId" ] int)



-- |  In production should be empty, for elm-live with hot reload


prefix : String
prefix = "/elm-live"


apiLogin : String
apiLogin =
    prefix ++ "/api/v1/login"


apiLogout : String
apiLogout =
    prefix ++ "/api/v1/logout"


apiGetLatestCurrentCases : String
apiGetLatestCurrentCases =
    prefix ++ "/api/v1/services/current"


apiGetLatestClosingCases : String
apiGetLatestClosingCases =
    prefix ++ "/api/v1/services/closing"


apiGetCaseComments : String
apiGetCaseComments =
    prefix ++ "/api/v1/getServiceComments/"


apiPostServiceComment : Int -> String
apiPostServiceComment service =
    prefix ++ "/api/v1/case/" ++ String.fromInt service ++ "/comment"


apiGetService : String
apiGetService =
    prefix ++ "/api/v1/getService/"


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


latestCurrentCasesDecoder : Decoder (List CurrentCaseInfo)
latestCurrentCasesDecoder =
    let
        decodeCurrentCaseInfo : Decoder CurrentCaseInfo
        decodeCurrentCaseInfo =
            succeed CurrentCaseInfo
                |> required "cuCaseId" int
                |> required "cuServiceId" int
                |> required "cuServiceSerial" int
                |> required "cuCallDate" (nullable ISO8601.decode)
                |> required "cuTypeOfService" string
                |> required "cuStatus" string
                |> required "cuAccordTime" string
                |> required "cuMakeModel" string
                |> required "cuBreakdownPlace" string
                |> required "cuPayType" string
    in
    list decodeCurrentCaseInfo


latestClosingCasesDecoder : Decoder (List ClosingCaseInfo)
latestClosingCasesDecoder =
    let
        decodeClosingCaseInfo : Decoder ClosingCaseInfo
        decodeClosingCaseInfo =
            succeed ClosingCaseInfo
                |> required "clCaseId" int
                |> required "clServiceId" int
                |> required "clServiceSerial" int
                |> required "clCallDate" (nullable ISO8601.decode)
                |> required "clTypeOfService" string
                |> required "clMakeModel" string
                |> required "clBreakdownPlace" string
                |> required "clPayType" string
    in
    list decodeClosingCaseInfo


getLatestCurrentCases : (Result Http.Error (List CurrentCaseInfo) -> msg) -> Cmd msg
getLatestCurrentCases message =
    HttpBuilder.post apiGetLatestCurrentCases
        |> HttpBuilder.withExpect (Http.expectJson message latestCurrentCasesDecoder)
        |> HttpBuilder.request


getLatestClosingCases : (Result Http.Error (List ClosingCaseInfo) -> msg) -> Cmd msg
getLatestClosingCases message =
    HttpBuilder.post apiGetLatestClosingCases
        |> HttpBuilder.withExpect (Http.expectJson message latestClosingCasesDecoder)
        |> HttpBuilder.request


getService : Int -> (Result Http.Error ServiceDescription -> msg) -> Cmd msg
getService serviceId message =
    let
        getCaseDecoder : Decoder ServiceDescription
        getCaseDecoder =
            succeed ServiceDescription
                |> required "caseId" int
                |> optional "services" int 0
                |> optional "serviceType" string ""
                |> required "status" int
                |> required "statusLabel" string
                |> optional "client" string ""
                |> optional "clientPhone" string ""
                |> optional "firstAddress" string ""
                |> optional "lastAddress" string ""
                |> required "expectedServiceStart" (nullable ISO8601.decode)
                |> required "factServiceStart" (nullable ISO8601.decode)
                |> required "factServiceEnd" (nullable ISO8601.decode)
                |> optional "makeModel" string ""
                |> optional "plateNumber" string ""
                |> required "loadingDifficulties" (nullable (dict (nullable bool)))
                |> optional "suburbanMilage" string ""
                |> required "vin" (nullable string)
    in
    HttpBuilder.get (apiGetService ++ String.fromInt serviceId)
        |> HttpBuilder.withExpect (Http.expectJson message getCaseDecoder)
        |> HttpBuilder.request


getCaseComments : Int -> (Result Http.Error (List CaseComment) -> msg) -> Cmd msg
getCaseComments caseId message =
    let
        actionDecoder : Decoder CaseCommentDetails
        actionDecoder =
            succeed
                (\t r c s ->
                    CaseCommentAction
                        { type_ = t
                        , result = r
                        , comment = c
                        , serviceLabel = s
                        }
                )
                |> required "actiontype" string
                |> required "actionresult" string
                |> required "actioncomment" (nullable string)
                |> optional "servicelabel" (nullable string) Nothing

        callDecoder : Decoder CaseCommentDetails
        callDecoder =
            succeed (\t -> CaseCommentCall { callType = t })
                |> required "calltype" string

        commentDecoder : Decoder CaseCommentDetails
        commentDecoder =
            succeed (\t -> CaseCommentComment { commentText = t })
                |> required "commenttext" string

        avayaEventDecoder : Decoder CaseCommentDetails
        avayaEventDecoder =
            succeed
                (\t c i ->
                    CaseCommentAvayaEvent
                        { aeType = t
                        , aeCall = c
                        , aeInterLocutors = i
                        }
                )
                |> required "aetype" string
                |> required "aecall" string
                |> required "aeinterlocutors" (list string)

        partnerCancelDecoder : Decoder CaseCommentDetails
        partnerCancelDecoder =
            succeed
                (\n c r ->
                    CaseCommentPartnerCancel
                        { partnerName = n
                        , refusalComment = c
                        , refusalReason = r
                        }
                )
                |> required "partnername" string
                |> required "refusalcomment" string
                |> required "refusalreason" string

        partnerDelayDecoder : Decoder CaseCommentDetails
        partnerDelayDecoder =
            succeed
                (\n l m c ->
                    CaseCommentPartnerDelay
                        { partnerName = n
                        , serviceLabel = l
                        , delayMinutes = m
                        , delayConfirmed = c
                        }
                )
                |> required "partnername" string
                |> required "servicelabel" (nullable string)
                |> required "delayminutes" (nullable int)
                |> required "delayconfirmed" (nullable string)

        smsForPartnerDecoder : Decoder CaseCommentDetails
        smsForPartnerDecoder =
            succeed
                (\t s p d ->
                    CaseCommentSmsForPartner
                        { msgText = t
                        , sender = s
                        , phone = p
                        , deliveryStatus = d
                        }
                )
                |> required "msgtext" string
                |> required "sender" string
                |> required "phone" string
                |> required "deliverystatus" string

        locationSharingRequestDecoder : Decoder CaseCommentDetails
        locationSharingRequestDecoder =
            succeed
                (\smsSent ->
                    CaseCommentLocationSharingRequest
                        { smsSent = smsSent
                        }
                )
                |> required "smsSent" bool

        locationSharingResponseDecoder : Decoder CaseCommentDetails
        locationSharingResponseDecoder =
            succeed
                (\lat lon accuracy ->
                    CaseCommentLocationSharingResponse
                        { lat = lat
                        , lon = lon
                        , accuracy = accuracy
                        }
                )
                |> required "lat" float
                |> required "lon" float
                |> required "accuracy" int

        caseCommentJsonDecoder : Decoder CaseCommentDetails
        caseCommentJsonDecoder =
            field "type" string
                |> andThen
                    (\t ->
                        case t of
                            "action" ->
                                actionDecoder

                            "call" ->
                                callDecoder

                            "comment" ->
                                commentDecoder

                            "avayaEvent" ->
                                avayaEventDecoder

                            "partnerCancel" ->
                                partnerCancelDecoder

                            "partnerDelay" ->
                                partnerDelayDecoder

                            "smsForPartner" ->
                                smsForPartnerDecoder

                            "locationSharingRequest" ->
                                locationSharingRequestDecoder

                            "locationSharingResponse" ->
                                locationSharingResponseDecoder

                            _ ->
                                fail <|
                                    "Trying to decode case comment json, but type '"
                                        ++ t
                                        ++ "' is not supported."
                    )

        caseCommentsDecoder : Decoder (List CaseComment)
        caseCommentsDecoder =
            list caseCommentDecoder

        caseCommentDecoder : Decoder CaseComment
        caseCommentDecoder =
            succeed CaseComment
                |> required "datetime" (nullable ISO8601.decode)
                |> required "who" (nullable string)
                |> required "json" (nullable caseCommentJsonDecoder)
    in
    HttpBuilder.get (apiGetCaseComments ++ String.fromInt caseId)
        |> HttpBuilder.withExpect (Http.expectJson message caseCommentsDecoder)
        |> HttpBuilder.request


postServiceComment : Int -> { comment : String } -> (Result Http.Error Int -> msg) -> Cmd msg
postServiceComment service { comment } message =
    let
        postBody =
            [ ( "comment", comment )
            ]

        commentExpect : (Result Http.Error Int -> msg) -> Http.Expect msg
        commentExpect toMsg =
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
    HttpBuilder.post (apiPostServiceComment service)
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (commentExpect message)
        |> HttpBuilder.request
