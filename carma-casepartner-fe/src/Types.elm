module Types exposing (CaseDescription, CaseInfo)

-- заявка


type alias CaseInfo =
    { id : Int -- идентификатор заявка
    , services : Int -- Количество услуг
    , callDate : String -- дата
    , typeOfService : String -- тип услуги
    , status : String -- статус
    , accordTime : String -- овноу
    , remainTime : String -- остаток времени (таймер)
    , makeModel : String -- марка/модель
    , breakdownPlace : String -- адрес места поломки
    , payType : String -- тип оплаты
    }


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
