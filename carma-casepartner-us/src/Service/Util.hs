module Service.Util
    ( carmaPostComment
    , carmaPostPartnerDelay
    , caseForService
    , serviceActionNotFound
    , serviceAlreadyPerformed
    , servicePerformed
    , setStatusClosed
    , setStatusInPlace
    , setStatusPartnerDelay
    , setStatusPerformed
    , withCookie
    ) where


import           Control.Monad                    (void, when)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BS
import           Data.Configurator                (lookupDefault)
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap
import           Snap.Snaplet.PostgresqlSimple    (Only (..), query)

import           Application
import           Carma.Model
import           Carma.Model.Action               as Action
import qualified Carma.Model.ActionType           as ActionType
import           Carma.Model.Service              as Service
import qualified Carma.Model.ServiceStatus        as ServiceStatus
import qualified CarmaApi
import qualified Data.Model.Patch                 as Patch


withCookie
    :: (MonadSnap (m b v), MonadSnaplet m)
    => (String -> m b v b1) -> m b v b1
withCookie f = do
  cfg <- getSnapletUserConfig

  cookieName :: String <- liftIO $ T.unpack . T.strip
                               <$> lookupDefault "_session" cfg "carma.cookie"

  c <- getCookie $ BS.pack cookieName

  case c of
    Nothing -> f ""
    Just c' -> f $ cookieName ++ "=" ++ BS.unpack (cookieValue c')


carmaUpdateFactServiceStartTime
    :: ( MonadSnaplet m
      , MonadSnap (m b v)
      )
    => IdentI Service
    -> m b v (Patch.Patch Service)
carmaUpdateFactServiceStartTime serviceId = withCookie $ \cookie -> do
  CarmaApi.updateFactServiceStartTime cookie serviceId


carmaServiceInProgress
    :: ( MonadSnaplet m
      , MonadSnap (m b v))
    => IdentI Action
    -> m b v (Patch.Patch Action)
carmaServiceInProgress actionId = withCookie $ \cookie -> do
  CarmaApi.serviceInProgress cookie actionId


carmaPostComment
    :: (MonadSnap (m b v), MonadSnaplet m)
    => Int
    -> String
    -> m b v Int
carmaPostComment caseId comment = withCookie $ \cookie -> do
  (Ident caseCommentId, _) <- CarmaApi.addComment cookie caseId comment
  return caseCommentId


carmaPostPartnerDelay
    :: (MonadSnap (m b v), MonadSnaplet m)
    => Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe Text
    -> m b v Int
carmaPostPartnerDelay caseId serviceId ownerId partnerId minutes reason comment =
  withCookie $ \cookie -> do
    (Ident partnerDelayId, _) <- CarmaApi.addPartnerDelay cookie
                                                         caseId serviceId
                                                         ownerId partnerId
                                                         minutes reason comment
    return partnerDelayId


-- Returns caseId for specified serviceId
caseForService :: Int -> AppHandler (Maybe Int)
caseForService serviceId =
  query "SELECT parentid FROM servicetbl where id = ?" (Only serviceId) >>=
  (return . \case
              [Only caseId] -> Just caseId
              _             -> Nothing)


setStatusClosed
    :: IdentI Service
    -> IdentI Action
    -> AppHandler (M.Map Text Text)
setStatusClosed serviceId checkActionId = do
  void $ withCookie $ \cookie ->
      CarmaApi.serviceClosed cookie serviceId checkActionId
  return serviceClosed


setStatusInPlace :: Int -> AppHandler (M.Map Text Text)
setStatusInPlace serviceId = do
  [(caseId, status)] <- query
                       "SELECT parentid, status FROM servicetbl WHERE id = ?" $
                       Only serviceId
  if status == ServiceStatus.ordered
  then do assigned <- isServiceAssignedToOperator serviceId
          if assigned -- назначена ли услуга на оператора
          then do
            _ <- carmaPostComment caseId "Партнёром произведён доезд до клиента"
            return servicePerformed
          else do
            -- происходит выполнения действия "Услуга в процессе оказания"
            v <- query [sql|
              SELECT id
              FROM actiontbl
              WHERE serviceId = ?
                AND type = ?
                AND result is null
            |] (serviceId, ActionType.checkStatus)

            case v of
              [Only (actionId :: IdentI Action)] -> do
                     _ <- carmaServiceInProgress actionId
                     -- update servicetbl.times в значение now
                     _ <- carmaUpdateFactServiceStartTime $ Ident serviceId
                     return servicePerformed
              _ -> return serviceActionNotFound

  else if status == ServiceStatus.ok
       then return serviceAlreadyPerformed
       else do
         _ <- carmaPostComment caseId "Партнёр приступил к оказанию услуги"
         return servicePerformed



setStatusPerformed :: Int -> String -> AppHandler (M.Map Text Text)
setStatusPerformed serviceId comment = do
  Just caseId <- caseForService serviceId
  _ <- carmaPostComment caseId comment
  return servicePerformed


setStatusPartnerDelay
    :: Int
    -> Int
    -> Int
    -> Int
    -> Maybe Text
    -> AppHandler Int
setStatusPartnerDelay serviceId uid minutes reason comment = do
  [Only (partnerId :: Int)] <- query
    "SELECT contractor_partnerid FROM servicetbl where id = ?"
    $ Only serviceId
  Just caseId <- caseForService serviceId
  when (minutes < 1) $ error "minutes should be > 0"

  carmaPostPartnerDelay caseId serviceId uid partnerId minutes reason comment


-- Услуга закрыта
serviceClosed :: M.Map Text Text
serviceClosed = serviceMessage "service_closed"

-- Услуга выполнена
servicePerformed :: M.Map Text Text
servicePerformed = serviceMessage "service_performed"

-- Услуга уже выполнена
serviceAlreadyPerformed :: M.Map Text Text
serviceAlreadyPerformed = serviceMessage "service_already_performed"

-- Действие не найдено
serviceActionNotFound :: M.Map Text Text
serviceActionNotFound = serviceMessage "action_not_found"

serviceMessage :: Text -> M.Map Text Text
serviceMessage message = M.fromList [ ( "status" :: Text
                                      , message
                                      )
                                    ]


isServiceAssignedToOperator :: Int -> AppHandler Bool
isServiceAssignedToOperator serviceId = do
  [Only (count :: Int)] <- query [sql|
    SELECT count(*)
    FROM actiontbl
    WHERE serviceid = ?
      AND result IS NULL
      AND assignedto IS NOT NULL
      AND type = ?
  |] (serviceId, ActionType.checkStatus)
  return $ count > 0
