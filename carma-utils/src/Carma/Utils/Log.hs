module Carma.Utils.Log
    ( Syslog.Priority (..)
    , hushExceptions
    , logExceptions
    , syslogJSON
    , syslogTxt
    ) where

import           Control.Concurrent            (myThreadId)
import qualified Control.Exception             as Ex
import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L8
import qualified Data.Text                     as T
import           Foreign.C.Types               (CInt)
import qualified System.Posix.Syslog           as Syslog
import qualified System.Posix.Syslog.Functions as Syslog


syslogTxt :: MonadIO m => Syslog.Priority -> String -> String -> m ()
syslogTxt p tag msg = syslogJSON p tag ["msg" .= msg]


syslogJSON :: MonadIO m => Syslog.Priority -> String -> [Aeson.Pair] -> m ()
syslogJSON p tag msg = liftIO $ do
  tid <- myThreadId
  let msg' = ("tid" .= show tid) : msg
  let msgBS = L8.concat -- FIXME: escape '%' in messge
        [L8.pack tag, " ", Aeson.encode $ Aeson.object msg']
  B.useAsCString (L8.toStrict msgBS)
    $ flip (Syslog._syslog (toEnum (fromEnum Syslog.User))
        (toEnum (fromEnum p)))
            (fromIntegral (L8.length msgBS) :: CInt)


hushExceptions :: (MonadIO m, MonadBaseControl IO m) => String -> m () -> m ()
hushExceptions tag act = catch act $ \(e :: Ex.SomeException) ->
  syslogJSON Syslog.Warning tag
    ["msg" .= Aeson.String "hushed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]


logExceptions :: (MonadIO m, MonadBaseControl IO m) => String -> m a -> m a
logExceptions tag act = catch act $ \(e :: Ex.SomeException) -> do
  syslogJSON Syslog.Error tag
    ["msg" .= Aeson.String "rethrowed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]
  throw e
