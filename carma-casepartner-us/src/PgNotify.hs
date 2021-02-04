module PgNotify
    ( startLoop
    ) where


import           Control.Concurrent                      (forkIO, threadDelay)
import           Control.Exception                       (SomeException,
                                                          bracket, handle)
import           Control.Monad                           (forever, void)
import           Control.Monad.IO.Class                  (MonadIO, liftIO)
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString.Char8                   as BS8
import qualified Data.Configurator                       as Config
import qualified Data.Configurator.Types                 as Config
import qualified Data.Text                               as T
import           Database.PostgreSQL.Simple              (Only (Only))
import qualified Database.PostgreSQL.Simple              as PG
import qualified Database.PostgreSQL.Simple.Notification as PG

import           Carma.HTTP                              (CarmaIO,
                                                          CarmaOptions (..),
                                                          defaultCarmaOptions,
                                                          runCarma)
import           Carma.HTTP.New                          (createInstance)
import           Carma.Model                             (Ident (..), IdentI)
import           Carma.Model.CaseComment                 as CaseComment
import           Carma.Utils.Operators                   ((&))
import           Data.Model.Patch                        as Patch

import qualified CarmaApi


startLoop :: Config.Config -> IO ()
startLoop config = do
  pgUri <- liftIO $ Config.require config "pg-conn-str"

  void $ forkIO $ foreverWithDelay 2000
           $ bracket (PG.connectPostgreSQL pgUri) PG.close
             $ \c -> forever $ do
               -- todo: move listen identifier to config file
               void $ PG.execute_ c "LISTEN driver_service_response"
               n <- PG.getNotification c
               processResponse config c $ PG.notificationData n


foreverWithDelay :: Int -> IO () -> IO ()
foreverWithDelay ms =
  forever . handle (\(_ :: SomeException) -> threadDelay (ms *1000))


processResponse :: Config.Config -> PG.Connection -> ByteString -> IO ()
processResponse config c payload = do
  case BS8.readInt payload of
    Just (serviceId, _) -> do
      res :: [Only Int] <- PG.query c
        "SELECT parentId FROM servicetbl WHERE id = ?"
        $ PG.Only serviceId

      case res of
        (Only caseId : _) -> do
          message <- acceptedMessage config serviceId
          void $ liftIO $ addComment config caseId message

        _ -> liftIO $ print $ "driver_service_response: invalid caseId " ++ show res

    _ -> print $ "driver_service_response: invalid serviceId " ++ show payload


acceptedMessage :: Config.Config -> Int -> IO String
acceptedMessage config serviceId = do
  clientUri <- liftIO $ Config.require config "map.client-uri"
  return $ "Заявка принята в работу." ++
             "Местоположение водителя можно посмотреть по ссылке " ++
             BS8.unpack clientUri ++ "?serviceId=" ++ show serviceId


addComment :: MonadIO m
           => Config.Config
           -> Int
           -> String
           -> m (IdentI CaseComment, Patch CaseComment)
addComment config caseId comment = carma config $ createInstance cc
    where cc = Patch.empty
               & Patch.put CaseComment.caseId (Ident caseId)
               & Patch.put CaseComment.comment (T.pack comment)


carma :: Control.Monad.IO.Class.MonadIO m => Config.Config -> CarmaIO b -> m b
carma config action = do
  hostname :: String <- liftIO $
                      Config.lookupDefault CarmaApi.defaultHostname config "carma.host"
  port <- liftIO $ Config.lookupDefault CarmaApi.defaultPort config "carma.port"
  let carmaOptions = defaultCarmaOptions
                     { carmaHost = hostname
                     , carmaPort = port
                     }
  liftIO $ runCarma carmaOptions action
