module Types exposing (CaseDescription, CaseInfo)

import ISO8601 exposing (Time)



-- для списка заявок


type alias CaseInfo =
    { id : Int -- идентификатор заявка
    , services : Int -- Количество услуг
    , callDate : Maybe Time -- дата
    , typeOfService : String -- тип услуги
    , status : String -- статус
    , accordTime : Maybe Time -- овноу
    , makeModel : String -- марка/модель
    , breakdownPlace : String -- адрес места поломки
    , payType : String -- тип оплаты
    }



-- заявка


type alias CaseDescription =
    { caseId : Int
    , services : Int
    , serviceType : String
    , client : String
    , clientPhone : String
    , firstAddress : String
    , lastAddress : String
    , expectedServiceStart : String
    , factServiceStart : String
    , factServiceEnd : String
    , makeModel : String
    , plateNumber : String
    , loadingDifficulty : String
    , suburbanMilage : String
    }
