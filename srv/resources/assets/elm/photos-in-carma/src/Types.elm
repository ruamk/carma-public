module Types exposing
    ( Attachment
    , AttachmentId
    , Photo
    )


type alias Attachment =
    { hash : String
    , ctime : String
    , id : AttachmentId
    , filename : String
    }


type alias AttachmentId =
    Int


type alias Photo =
    { serviceId : Int
    , image : String
    , latitude : Float
    , longitude : Float
    , created : String
    , photoType : String
    }
