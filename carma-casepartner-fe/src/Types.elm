module Types exposing
    ( CaseComment
    , CaseCommentDetails(..)
    , ClosingCaseInfo
    , CurrentCaseInfo
    , Dictionary
    , ServiceDescription
    , ServiceInfo
    , emptyServiceDescription
    )

import Dict
import ISO8601 exposing (Time)



-- для списка заявок


type alias ServiceInfo =
    { caseId : Int
    , serviceId : Int
    , serviceSerial : Int
    , callDate : Maybe Time
    , typeOfService : String
    , makeModel : String
    , breakdownPlace : String
    , payType : String
    }


type alias CurrentCaseInfo =
    { cuCaseId : Int
    , cuServiceId : Int
    , cuServiceSerial : Int
    , cuCallDate : Maybe Time
    , cuTypeOfService : String
    , cuStatus : String
    , cuAccordTime : String
    , cuMakeModel : String
    , cuBreakdownPlace : String
    , cuPayType : String
    }


type alias ClosingCaseInfo =
    { clCaseId : Int
    , clServiceId : Int
    , clServiceSerial : Int
    , clCallDate : Maybe Time
    , clTypeOfService : String
    , clMakeModel : String
    , clBreakdownPlace : String
    , clPayType : String
    }



-- заявка


type alias ServiceDescription =
    { caseId : Int
    , services : Int
    , serviceType : String
    , status : Int
    , statusLabel : String
    , client : String
    , clientPhone : String
    , firstAddress : String
    , lastAddress : String
    , expectedServiceStart : Maybe Time
    , factServiceStart : Maybe Time
    , factServiceEnd : Maybe Time
    , makeModel : String
    , plateNumber : String
    , loadingDifficulties : Maybe (Dict.Dict String (Maybe Bool))
    , suburbanMilage : String
    , vin : Maybe String
    }


emptyServiceDescription : ServiceDescription
emptyServiceDescription =
    { caseId = 0
    , services = 0
    , serviceType = ""
    , status = 0
    , statusLabel = ""
    , client = ""
    , clientPhone = ""
    , firstAddress = ""
    , lastAddress = ""
    , expectedServiceStart = Nothing
    , factServiceStart = Nothing
    , factServiceEnd = Nothing
    , makeModel = ""
    , plateNumber = ""
    , loadingDifficulties = Nothing
    , suburbanMilage = ""
    , vin = Nothing
    }


type CaseCommentDetails
    = CaseCommentAction
        { type_ : String
        , result : String
        , comment : Maybe String
        , serviceLabel : Maybe String
        }
    | CaseCommentAvayaEvent
        { aeType : String
        , aeCall : String
        , aeInterLocutors : List String
        }
    | CaseCommentCall
        { callType : String
        }
    | CaseCommentComment
        { commentText : String
        }
    | CaseCommentLocationSharingResponse
        { lat : Float
        , lon : Float
        , accuracy : Int
        }
    | CaseCommentLocationSharingRequest
        { smsSent : Bool
        }
    | CaseCommentPartnerCancel
        { partnerName : String
        , refusalComment : String
        , refusalReason : String
        }
    | CaseCommentPartnerDelay
        { partnerName : String
        , serviceLabel : Maybe String
        , delayMinutes : Maybe Int
        , delayConfirmed : Maybe String
        }
    | CaseCommentSmsForPartner
        { msgText : String
        , sender : String
        , phone : String
        , deliveryStatus : String
        }


type alias CaseComment =
    { datetime : Maybe Time
    , who : Maybe String
    , details : Maybe CaseCommentDetails
    }


type alias Dictionary =
    Dict.Dict String String
