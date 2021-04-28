module Util
    ( -- * JSON responses
      customOkResponse
    , errorResponse
    , okResponse
    ) where


import           Data.Aeson       as Aeson
import           Data.Text        (Text)

import           Application      (AppHandler)
import           Carma.Utils.Snap (writeJSON)


response :: Text -> Value -> Value
response s m = object [ ("status",  String s)
                      , ("message", m)
                      ]


okResponse :: Text -> AppHandler ()
okResponse message = writeJSON $ response "ok" $ String message


errorResponse :: Text -> AppHandler ()
errorResponse message = writeJSON $ response "error" $ String message


customOkResponse :: Value -> AppHandler ()
customOkResponse value = writeJSON $ response "ok" value
