module Const exposing
    ( latenessReasons
    , serviceStatus
    )

{-| carma-models/src/Carma/Model/ServiceStatus.hs
-}


serviceStatus =
    { ordered = 15 -- Услуга заказана
    , inProgress = 17 -- Услуга оказывается
    , ok = 19 -- Услуга оказана
    }


latenessReasons =
    { other = 1
    }


paymentType = 
    { ruamc = 1
    , client = 2
    , mixed = 3
    , refund = 4
    }