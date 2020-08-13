module Api exposing
    ( Session
    , decodeSession
    , getLatenessReasons
    , getLatestClosingCases
    , getLatestCurrentCases
    , getService
    , getServiceComments
    , getServices
    , login
    , postPartnerDelay
    , postServiceComment
    , statusInPlace
    , statusServicePerformed
    )

import Http
import HttpBuilder
import ISO8601
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
        , map3
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , ClosingCaseInfo
        , CurrentCaseInfo
        , Dictionary
        , ServiceDescription
        , ServiceInfo
        )


type alias Session =
    { username : String
    , serviceId : Int
    , route : String
    }



-- |  In production should be empty, for elm-live with hot reload


prefix : String
prefix =
    "/elm-live"


apiLogin : String
apiLogin =
    prefix ++ "/api/v1/login"


apiLogout : String
apiLogout =
    prefix ++ "/api/v1/logout"


apiGetServices : String
apiGetServices =
    prefix ++ "/api/v1/services/all"


apiGetLatestCurrentCases : String
apiGetLatestCurrentCases =
    prefix ++ "/api/v1/services/current"


apiGetLatestClosingCases : String
apiGetLatestClosingCases =
    prefix ++ "/api/v1/services/closing"


apiGetServiceComments : Int -> String
apiGetServiceComments serviceId =
    prefix
        ++ "/api/v1/service/"
        ++ String.fromInt serviceId
        ++ "/comments"


apiPostServiceComment : Int -> String
apiPostServiceComment serviceId =
    prefix ++ "/api/v1/case/" ++ String.fromInt serviceId ++ "/comment"


apiPostPartnerDelay : Int -> String
apiPostPartnerDelay serviceId =
    prefix ++ "/api/v1/service/" ++ String.fromInt serviceId ++ "/partnerdelay"


apiGetService : Int -> String
apiGetService serviceId =
    prefix ++ "/api/v1/service/" ++ String.fromInt serviceId


apiStatusInPlace : Int -> String
apiStatusInPlace serviceId =
    prefix ++ "/api/v1/service/" ++ String.fromInt serviceId ++ "/inplace"


apiStatusServicePerformed : Int -> String
apiStatusServicePerformed serviceId =
    prefix ++ "/api/v1/service/" ++ String.fromInt serviceId ++ "/performed"


apiGetLatenessReasons : String
apiGetLatenessReasons =
    prefix ++ "/api/v1/dict/PartnerDelay_Reason"


decodeSession : String -> Session
decodeSession s =
    case decodeString sessionDecoder s of
        Ok session ->
            session

        Err _ ->
            Session "" 0 ""


sessionDecoder : Decoder Session
sessionDecoder =
    map3 Session
        (at [ "username" ] string)
        (at [ "serviceId" ] int)
        (at [ "route" ] string)


login : String -> String -> (Result Http.Error Int -> msg) -> Cmd msg
login name password message =
    let
        postBody : List ( String, String )
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

                        Http.BadStatus_ metadata _ ->
                            Ok metadata.statusCode

                        Http.GoodStatus_ metadata _ ->
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


servicesDecoder : Decoder (List ServiceInfo)
servicesDecoder =
    let
        decodeServiceInfo : Decoder ServiceInfo
        decodeServiceInfo =
            succeed ServiceInfo
                |> required "caseId" int
                |> required "serviceId" int
                |> required "serviceSerial" int
                |> required "callDate" (nullable ISO8601.decode)
                |> required "typeOfService" string
                |> required "makeModel" string
                |> required "breakdownPlace" string
                |> required "payType" string
    in
    list decodeServiceInfo


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


getServices : Int -> Int -> (Result Http.Error (List ServiceInfo) -> msg) -> Cmd msg
getServices offset limit message =
    let
        postBody : List ( String, String )
        postBody =
            [ ( "offset", String.fromInt offset )
            , ( "limit", String.fromInt limit )
            ]
    in
    HttpBuilder.post apiGetServices
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (Http.expectJson message servicesDecoder)
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
    HttpBuilder.get (apiGetService serviceId)
        |> HttpBuilder.withExpect (Http.expectJson message getCaseDecoder)
        |> HttpBuilder.request


getServiceComments : Int -> (Result Http.Error (List CaseComment) -> msg) -> Cmd msg
getServiceComments serviceId message =
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
    HttpBuilder.get (apiGetServiceComments serviceId)
        |> HttpBuilder.withExpect (Http.expectJson message caseCommentsDecoder)
        |> HttpBuilder.request


postServiceComment : Int -> { comment : String } -> (Result Http.Error Int -> msg) -> Cmd msg
postServiceComment service { comment } message =
    let
        postBody : List ( String, String )
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

                        Http.BadStatus_ metadata _ ->
                            Ok metadata.statusCode

                        Http.GoodStatus_ metadata _ ->
                            Ok metadata.statusCode
    in
    HttpBuilder.post (apiPostServiceComment service)
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (commentExpect message)
        |> HttpBuilder.request


postPartnerDelay :
    Int
    -> { minutes : Int, reason : Int, comment : Maybe String }
    -> (Result Http.Error Int -> msg)
    -> Cmd msg
postPartnerDelay service { minutes, reason, comment } message =
    let
        params : List ( String, String )
        params =
            [ ( "minutes", String.fromInt minutes )
            , ( "reason", String.fromInt reason )
            ]
                ++ (case comment of
                        Just comment_ ->
                            [ ( "comment", comment_ ) ]

                        Nothing ->
                            []
                   )

        expect : (Result Http.Error Int -> msg) -> Http.Expect msg
        expect toMsg =
            Http.expectStringResponse toMsg <|
                \response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err (Http.BadUrl url)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Ok metadata.statusCode

                        Http.GoodStatus_ metadata _ ->
                            Ok metadata.statusCode
    in
    HttpBuilder.post (apiPostPartnerDelay service)
        |> HttpBuilder.withUrlEncodedBody params
        |> HttpBuilder.withExpect (expect message)
        |> HttpBuilder.request


statusDecoder : Decoder String
statusDecoder =
    field "status" string


statusExpect : (Result Http.Error String -> msg) -> Http.Expect msg
statusExpect toMsg =
    Http.expectJson toMsg statusDecoder


statusInPlace : Int -> (Result Http.Error String -> msg) -> Cmd msg
statusInPlace serviceId message =
    HttpBuilder.post (apiStatusInPlace serviceId)
        |> HttpBuilder.withExpect (statusExpect message)
        |> HttpBuilder.request


statusServicePerformed : Int -> String -> (Result Http.Error String -> msg) -> Cmd msg
statusServicePerformed serviceId comment message =
    let
        postBody : List ( String, String )
        postBody =
            [ ( "comment", comment ) ]
    in
    HttpBuilder.post (apiStatusServicePerformed serviceId)
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (statusExpect message)
        |> HttpBuilder.request


dictionaryDecoder : Decoder Dictionary
dictionaryDecoder =
    dict string


getLatenessReasons : (Result Http.Error Dictionary -> msg) -> Cmd msg
getLatenessReasons message =
    HttpBuilder.get apiGetLatenessReasons
        |> HttpBuilder.withExpect (Http.expectJson message dictionaryDecoder)
        |> HttpBuilder.request
