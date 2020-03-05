module Types exposing (TheCase)

-- заявка


type alias TheCase =
    { id : String -- идентификатор заявка
    , callDate : String -- дата
    , typeOfService : String -- тип услуги
    , status : String -- статус
    , accordTime : String -- овноу
    , remainTime : String -- остаток времени (таймер)
    , makeModel : String -- марка/модель
    , breakdownPlace : String -- адрес места поломки
    , payType : String -- тип оплаты
    }
