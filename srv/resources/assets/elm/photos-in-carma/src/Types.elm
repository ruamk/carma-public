module Types 
    exposing 
        ( Attachment
        , AttachmentId
        )


type alias Attachment = 
    { hash : String
    , ctime : String
    , id : AttachmentId
    , filename : String
    }

type alias AttachmentId = Int