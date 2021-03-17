module Api exposing
    ( SearchCondition(..)
    , Session
    , assignServiceToDriver
    , cancelServiceToDriver
    , clientMapURL
    , closeService
    , createDriver
    , decodeSession
    , deleteDriver
    , getDrivers
    , getLatenessReasons
    , getLatestClosingCases
    , getLatestCurrentCases
    , getPhotos
    , getService
    , getServiceComments
    , getServices
    , getTypeOfServiceSynonym
    , inviteDriver
    , login
    , postPartnerDelay
    , postServiceComment
    , savePhoto
    , statusInPlace
    , statusServicePerformed
    , updateDriver
    )

import File
import Http
import HttpBuilder
import ISO8601
import Json.Decode as D
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
        , index
        , int
        , list
        , map3
        , nullable
        , string
        , succeed
        )
import Json.Decode.Pipeline exposing (optional, required)
import Maybe
import Types
    exposing
        ( CaseComment
        , CaseCommentDetails(..)
        , ClosingCaseInfo
        , CurrentCaseInfo
        , Dictionary
        , Driver
        , Location
        , Payment
        , Photo
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
    ""


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


apiGetTypeOfServiceSynonym : String
apiGetTypeOfServiceSynonym =
    prefix ++ "/api/v1/dict/TypeOfServiceSynonym"


apiGetDrivers : String
apiGetDrivers =
    prefix ++ "/api/v1/settings/drivers"


apiCreateDriver : String
apiCreateDriver =
    prefix ++ "/api/v1/settings/driver"


apiDriver : Int -> String
apiDriver driverId =
    prefix ++ "/api/v1/settings/driver/" ++ String.fromInt driverId


apiAssignServiceToDriver : Int -> Int -> String
apiAssignServiceToDriver serviceId driverId =
    prefix
        ++ "/api/v1/assignservice/"
        ++ String.fromInt serviceId
        ++ "/"
        ++ String.fromInt driverId


apiCancelServiceToDriver : Int -> Int -> String
apiCancelServiceToDriver serviceId driverId =
    prefix
        ++ "/api/v1/cancelservice/"
        ++ String.fromInt serviceId
        ++ "/"
        ++ String.fromInt driverId


apiCloseService : Int -> String
apiCloseService serviceId =
    prefix
        ++ "/api/v1/service/"
        ++ String.fromInt serviceId
        ++ "/closed"


apiGetPhotos : Int -> String
apiGetPhotos serviceId =
    prefix
        ++ "/api/v1/service/"
        ++ String.fromInt serviceId
        ++ "/photo"


apiSavePhoto : Int -> String
apiSavePhoto serviceId =
    prefix
        ++ "/api/v1/service/"
        ++ String.fromInt serviceId
        ++ "/photo"


apiGetDriverImage : String -> String
apiGetDriverImage imageUrl =
    prefix ++ imageUrl


clientMapURL : Int -> String
clientMapURL serviceId =
    prefix ++ "/map-client.html?serviceId=" ++ String.fromInt serviceId


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


closeService : Int -> Float -> String -> Float -> (Result Http.Error Bool -> msg) -> Cmd msg
closeService serviceId partner partnerTranscript client message =
    let
        body =
            [ ( "partner", String.fromFloat partner )
            , ( "partnerTranscript", partnerTranscript )
            , ( "client", String.fromFloat client )
            ]

        decoder =
            D.map (\s -> s == "service_closed") (field "status" string)
    in
    HttpBuilder.post (apiCloseService serviceId)
        |> HttpBuilder.withUrlEncodedBody body
        |> HttpBuilder.withExpect (Http.expectJson message decoder)
        |> HttpBuilder.request


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
                |> required "cuTypeOfService" (nullable string)
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
                |> required "clTypeOfService" (nullable string)
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
                |> required "typeOfService" (nullable string)
                |> required "makeModel" string
                |> required "breakdownPlace" string
                |> required "payType" string
                |> required "status" string
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


type SearchCondition
    = SearchAll
    | SearchServiceId String
    | SearchCallDate ( String, String )


getServices : SearchCondition -> Int -> Int -> (Result Http.Error (List ServiceInfo) -> msg) -> Cmd msg
getServices condition offset limit message =
    let
        postBody : List ( String, String )
        postBody =
            (case condition of
                SearchAll ->
                    []

                SearchServiceId i ->
                    [ ( "serviceId", i ) ]

                SearchCallDate ( s, e ) ->
                    (if String.isEmpty s then
                        []

                     else
                        [ ( "callDateStart", s ) ]
                    )
                        ++ (if String.isEmpty e then
                                []

                            else
                                [ ( "callDateEnd", e ) ]
                           )
            )
                ++ [ ( "offset", String.fromInt offset )
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
                |> required "vin" (nullable string)
                |> required "payType" (nullable int)
                |> required "payment" (nullable payment)
                |> required "firstLocation" (nullable location)
                |> required "lastLocation" (nullable location)

        payment : Decoder Payment
        payment =
            succeed Payment
                |> required "partnerCost" (nullable float)
                |> required "checkCost" (nullable float)
                |> required "partnerCostTranscript" (nullable string)
                |> required "checkCostTranscript" (nullable string)
                |> required "paidByClient" (nullable string)

        location : Decoder Location
        location =
            succeed Location
                |> required "latitude" (nullable float)
                |> required "longitude" (nullable float)
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
            (case comment of
                Just comment_ ->
                    [ ( "comment", comment_ ) ]

                Nothing ->
                    []
            )
                ++ [ ( "minutes", String.fromInt minutes )
                   , ( "reason", String.fromInt reason )
                   ]

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


getDictionary : String -> (Result Http.Error Dictionary -> msg) -> Cmd msg
getDictionary url message =
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson message dictionaryDecoder)
        |> HttpBuilder.request


getLatenessReasons : (Result Http.Error Dictionary -> msg) -> Cmd msg
getLatenessReasons message =
    getDictionary apiGetLatenessReasons message


getTypeOfServiceSynonym : (Result Http.Error Dictionary -> msg) -> Cmd msg
getTypeOfServiceSynonym message =
    getDictionary apiGetTypeOfServiceSynonym message


getDrivers : (Result Http.Error (List Driver) -> msg) -> Cmd msg
getDrivers message =
    let
        driversDecoder =
            list driverDecoder

        driverDecoder : Decoder Driver
        driverDecoder =
            succeed Driver
                |> required "id" int
                |> required "partnerId" int
                |> required "phone" string
                |> required "password" string
                |> required "name" string
                |> required "plateNum" (nullable string)
                |> required "isActive" bool
                |> required "serviceId" (nullable int)
    in
    HttpBuilder.get apiGetDrivers
        |> HttpBuilder.withExpect (Http.expectJson message driversDecoder)
        |> HttpBuilder.request


createDriver : Driver -> (Result Http.Error Int -> msg) -> Cmd msg
createDriver driver message =
    let
        postBody : List ( String, String )
        postBody =
            [ ( "phone", driver.phone )
            , ( "password", driver.password )
            , ( "name", driver.name )
            , ( "plateNum"
              , case driver.plateNum of
                    Just plateNum ->
                        plateNum

                    Nothing ->
                        ""
              )
            , ( "isActive"
              , if driver.isActive then
                    "true"

                else
                    "false"
              )
            ]

        expect : (Result Http.Error Int -> msg) -> Http.Expect msg
        expect toMsg =
            Http.expectStringResponse toMsg <|
                \response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err <| Http.BadUrl url

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err <| Http.BadStatus metadata.statusCode

                        Http.GoodStatus_ metadata _ ->
                            Ok metadata.statusCode
    in
    HttpBuilder.post apiCreateDriver
        |> HttpBuilder.withUrlEncodedBody postBody
        |> HttpBuilder.withExpect (expect message)
        |> HttpBuilder.request


deleteDriver : Int -> (Result Http.Error Int -> msg) -> Cmd msg
deleteDriver driverId message =
    let
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
    HttpBuilder.delete (apiDriver driverId)
        |> HttpBuilder.withExpect (expect message)
        |> HttpBuilder.request


inviteDriver : Int -> (Result Http.Error Int -> msg) -> Cmd msg
inviteDriver driverId message =
    let
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
    HttpBuilder.post (apiDriver driverId)
        |> HttpBuilder.withExpect (expect message)
        |> HttpBuilder.request


updateDriver : Driver -> (Result Http.Error Int -> msg) -> Cmd msg
updateDriver driver message =
    let
        putBody : List ( String, String )
        putBody =
            [ ( "name", driver.name )
            , ( "phone", driver.phone )
            , ( "password", driver.password )
            , ( "plateNum"
              , case driver.plateNum of
                    Just plateNum ->
                        plateNum

                    Nothing ->
                        ""
              )
            , ( "isActive"
              , if driver.isActive then
                    "true"

                else
                    "false"
              )
            ]

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
    HttpBuilder.put (apiDriver driver.id)
        |> HttpBuilder.withUrlEncodedBody putBody
        |> HttpBuilder.withExpect (expect message)
        |> HttpBuilder.request


assignServiceToDriver : Int -> Int -> (Result Http.Error Int -> msg) -> Cmd msg
assignServiceToDriver serviceId driverId message =
    let
        queueIdDecoder : Decoder Int
        queueIdDecoder =
            int
    in
    HttpBuilder.get (apiAssignServiceToDriver serviceId driverId)
        |> HttpBuilder.withExpect (Http.expectJson message queueIdDecoder)
        |> HttpBuilder.request


cancelServiceToDriver : Int -> Int -> (Result Http.Error Int -> msg) -> Cmd msg
cancelServiceToDriver serviceId driverId message =
    let
        queueIdDecoder : Decoder Int
        queueIdDecoder =
            int
    in
    HttpBuilder.get (apiCancelServiceToDriver serviceId driverId)
        |> HttpBuilder.withExpect (Http.expectJson message queueIdDecoder)
        |> HttpBuilder.request


getPhotos : Int -> (Result Http.Error (Result String (List Photo)) -> msg) -> Cmd msg
getPhotos serviceId message =
    let
        photoDecoder =
            succeed Photo
                |> required "serviceId" int
                |> required "image" (D.map apiGetDriverImage string)
                |> required "latitude" float
                |> required "longitude" float
                |> required "created" string
                |> required "type" string

        decoder =
            let
                handleStatus status =
                    case status of
                        "ok" ->
                            D.map Ok <| field "message" (list photoDecoder)

                        err ->
                            D.map Err <| succeed err
            in
            field "status" string
                |> D.andThen handleStatus
    in
    HttpBuilder.get (apiGetPhotos serviceId)
        |> HttpBuilder.withExpect (Http.expectJson message decoder)
        |> HttpBuilder.request


savePhoto : Int -> File.File -> String -> (Result Http.Error (Result String Int) -> msg) -> Cmd msg
savePhoto serviceId photo photoType message =
    let
        decoder =
            let
                handle status msg =
                    case status of
                        "ok" ->
                            case String.toInt msg of
                                Just n ->
                                    Ok n

                                Nothing ->
                                    Err "Decoding error: `message` must be an integer"

                        "error" ->
                            Err msg

                        _ ->
                            Err "unexpected status"
            in
            D.map2
                handle
                (field "status" string)
                (field "message" string)

        body =
            [ Http.filePart "image" photo
            , Http.stringPart "serviceId" (String.fromInt serviceId)
            , Http.stringPart "latitude" (String.fromFloat 0)
            , Http.stringPart "longitude" (String.fromFloat 0)
            , Http.stringPart "created" (File.lastModified photo |> ISO8601.fromPosix |> ISO8601.toString)
            , Http.stringPart "type" photoType
            ]
    in
    Http.post
        { url = apiSavePhoto serviceId
        , body = Http.multipartBody body
        , expect = Http.expectJson message decoder
        }
