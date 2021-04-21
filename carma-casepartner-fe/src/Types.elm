module Types exposing
    ( CaseComment
    , CaseCommentDetails(..)
    , ClosingCaseInfo
    , CurrentCaseInfo
    , Dictionary
    , Driver
    , Location
    , Payment
    , PaymentType(..)
    , Photo
    , ServiceDescription
    , ServiceInfo
    , CarInfo
    , DriverLocation
    , emptyServiceDescription
    )

import Dict
import File exposing (File)
import ISO8601 exposing (Time)



-- для списка заявок


type alias ServiceInfo =
    { caseId : Int
    , serviceId : Int
    , serviceSerial : Int
    , callDate : Maybe Time
    , typeOfService : Maybe String
    , makeModel : String
    , breakdownPlace : String
    , payType : String
    , status : String
    }


type alias CurrentCaseInfo =
    { cuCaseId : Int
    , cuServiceId : Int
    , cuServiceSerial : Int
    , cuCallDate : Maybe Time
    , cuTypeOfService : Maybe String
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
    , clTypeOfService : Maybe String
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
    , vin : Maybe String
    , payType : Maybe Int
    , payment : Maybe Payment
    , firstLocation : Maybe Location
    , lastLocation : Maybe Location
    }


type alias Payment =
    { partnerCost : Maybe Float
    , checkCost : Maybe Float
    , partnerCostTranscript : Maybe String
    , checkCostTranscript : Maybe String
    , paidByClient : Maybe String
    }


type alias Location =
    { latitude : Maybe Float
    , longitude : Maybe Float
    }


type PaymentType
    = RUAMK
    | Client
    | Mixed
    | Refund


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
    , vin = Nothing
    , payType = Nothing
    , payment = Nothing
    , firstLocation = Nothing
    , lastLocation = Nothing
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


type alias Driver =
    { id : Int
    , partner : Int
    , phone : String
    , password : String
    , name : String
    , plateNum : Maybe String
    , isActive : Bool
    , serviceId : Maybe Int
    }


type alias Photo =
    { serviceId : Int
    , image : String
    , latitude : Float
    , longitude : Float
    , created : String
    , photoType : String
    }

type alias DriverLocation =
    { id : Int
    , name : String
    , phone : String
    , plateNum : String
    , latitude : Float
    , longitude : Float
    , carInfo : Maybe CarInfo
    }

type alias CarInfo =
    { color : String
    , model : String
    }
