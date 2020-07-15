module AppHandlers.Service
    ( statusInPlace
    , statusServicePerformed
    , postComment
    , serviceComments
    , servicePerformed
    , handleApiGetService
    )
    where


import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON, Value, toJSON, genericToJSON)
import           Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Char8 as BS
import           Data.Configurator (lookupDefault)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.LocalTime (ZonedTime)
import           Database.PostgreSQL.Simple.FromField
                 (FromField, fromField, fromJSONField)
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap
import           Snap.Snaplet.PostgresqlSimple
                 ( query
                 , Only (..)
                 )

import           Application
import           AppHandlers.Users
import           AppHandlers.Util
import qualified CarmaApi
import           Carma.Model
import qualified Data.Model.Patch as Patch
import           Carma.Model.Action as Action
import qualified Carma.Model.ActionType as ActionType
import           Carma.Model.Service as Service
import qualified Carma.Model.ServiceStatus as ServiceStatus
import qualified Carma.Model.Usermeta as Usermeta
import           Snaplet.Auth.PGUsers -- (currentUserMetaId)


type LoadingDifficulties = M.Map String (Maybe Bool)

instance FromField LoadingDifficulties where
    fromField = fromJSONField

data CaseDescription = CaseDescription
    { _caseId :: Int
    , _services :: Int
    , _serviceType :: String
    , _status :: Int
    , _statusLabel :: String
    , _client :: String
    , _clientPhone :: String
    , _firstAddress :: String
    , _lastAddress :: String
    , _expectedServiceStart :: Maybe ZonedTime
    , _factServiceStart :: Maybe ZonedTime
    , _factServiceEnd :: Maybe ZonedTime
    , _makeModel :: String
    , _plateNumber :: String
    , _loadingDifficulties :: Maybe LoadingDifficulties
    , _suburbanMilage :: String
    , _vin :: Maybe String
    } deriving (Show, Generic)

instance ToJSON CaseDescription where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}


data CaseComment = CaseComment
    { _datetime :: Maybe ZonedTime
    , _who :: Maybe String
    , _json :: Maybe Value
    } deriving (Show, Generic)

instance ToJSON CaseComment where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

    
handleApiGetService :: AppHandler ()
handleApiGetService = do
  serviceId <- fromMaybe (error "invalid service id") <$>
              getIntParam "serviceId"
  [(caseId, client, clientPhone, firstAddress, makeModel, plateNumber, vin)] <- query [sql|
    SELECT
        casetbl.id
      , contact_name
      , contact_phone1
      , caseaddress_address
      , "CarMake".label || ' / ' ||
        regexp_replace("CarModel".label, '^([^/]*)/.*','\1')
      , car_platenum
      , car_vin
    FROM casetbl
    LEFT OUTER JOIN "CarMake"  ON "CarMake".id = car_make
    LEFT OUTER JOIN "CarModel" ON "CarModel".id = car_model
    WHERE casetbl.id IN (SELECT parentid FROM servicetbl where id = ?)
  |] $ Only serviceId

  [Only serviceSerial] <- query [sql|
    SELECT r
    FROM (
      SELECT row_number () OVER (PARTITION BY parentid ORDER BY id) AS r, id
      FROM servicetbl
      WHERE parentid = ? 
      ) AS a
    WHERE id = ?;
  |] (caseId, serviceId)

  [(expectedServiceStart, factServiceStart, factServiceEnd, serviceType
   , status, statusLabel)] <- query [sql|
    SELECT
        times_expectedservicestart
      , times_factservicestart
      , times_factserviceend
      , "ServiceType".label
      , status
      , "ServiceStatus".label
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" ON servicetbl.type = "ServiceType".id
    LEFT OUTER JOIN "ServiceStatus" ON servicetbl.status = "ServiceStatus".id
    WHERE servicetbl.id = ?
    ORDER by servicetbl.id DESC
    LIMIT 1
  |] $ Only serviceId

  r1 <- query [sql|
    SELECT coalesce(towaddress_address, '')
         , coalesce(suburbanmilage::text, '')
         , flags
    FROM allservicesview
    WHERE id = ?
    LIMIT 1
  |] $ Only serviceId
  let (lastAddress, suburbanMilage, loadingDifficulty) = if length r1 == 1
                                                         then head r1
                                                         else ("","", Nothing)

  writeJSON $ CaseDescription
               caseId serviceSerial serviceType
               status statusLabel
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               loadingDifficulty
               suburbanMilage
               vin


-- Returns caseid for specified serviceid
caseForService :: Int -> AppHandler (Maybe Int)
caseForService serviceId = do
  result <- query "SELECT parentid FROM servicetbl where id = ?" $
                 Only serviceId

  case result of
    [Only caseId] -> return $ Just caseId
    _             -> return   Nothing


serviceComments :: AppHandler ()
serviceComments = checkAuthCasePartner $ do
  Just user <- currentUserMeta
  let Just (Ident uid) = Patch.get user Usermeta.ident
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  Just caseId <- caseForService serviceId

  limit <- fromMaybe 100 <$> getIntParam "limit"
  rows <- query [sql|
    SELECT datetime, who, json
    FROM "CaseHistory"
    WHERE caseId = ?
      AND (json::jsonb @> '{"serviceid": ?}' OR
           json->>'userid' = ?)
    ORDER BY datetime DESC
    LIMIT ?
  |] (caseId, serviceId, show uid, limit)

  writeJSON $ map (\(datetime, who, json) -> CaseComment datetime who json)
                (rows :: [(Maybe ZonedTime, Maybe String, Maybe Value)])


-- POST ../:service/comment
postComment :: AppHandler ()
postComment = checkAuthCasePartner $ do
  -- user <- fromMaybe (error "No current user") <$> with auth currentUser
  caseId <- fromMaybe (error "invalid case id") <$> getIntParam "caseId"
  comment <- T.strip . TE.decodeUtf8 . fromMaybe "" <$> getParam "comment"

  when (T.null comment) (error "empty comment")

  commentId <- carmaPostComment caseId $ T.unpack comment
  writeJSON [commentId]


withCookie
    :: (MonadSnap (m b v), MonadSnaplet m)
    => (String -> m b v b1) -> m b v b1
withCookie f = do
  cfg <- getSnapletUserConfig

  cookieName :: String <- liftIO $ T.unpack . T.strip
                               <$> lookupDefault "_session" cfg "carma.cookie"

  c <- getCookie $ BS.pack cookieName

  case c of
    Nothing -> error $ "no cookie named " ++ cookieName
    Just c' -> f $ cookieName ++ "=" ++ BS.unpack (cookieValue c')


carmaPostComment
    :: (MonadSnap (m b v), MonadSnaplet m)
    => Int
    -> String
    -> m b v Int
carmaPostComment caseId comment = withCookie $ \cookie -> do
  (Ident caseCommentId, _) <- CarmaApi.addComment cookie caseId comment
  return caseCommentId


carmaServiceInProgress
    :: ( MonadSnaplet m
      , MonadSnap (m b v))
    => IdentI Action
    -> m b v (Patch.Patch Action)
carmaServiceInProgress actionId = withCookie $ \cookie -> do
  CarmaApi.serviceInProgress cookie actionId


carmaUpdateFactServiceStartTime
    :: ( MonadSnaplet m
      , MonadSnap (m b v)
      )
    => IdentI Service
    -> m b v (Patch.Patch Service)
carmaUpdateFactServiceStartTime serviceId = withCookie $ \cookie -> do
  CarmaApi.updateFactServiceStartTime cookie serviceId


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


-- | Сменить статус на "На месте"
-- Returns: {'status': 'service_performed'}
--          {'status': 'service_already_performed'}
statusInPlace :: AppHandler ()
statusInPlace = checkAuthCasePartner $ do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  [(caseId, status)] <- query
                       "SELECT parentid, status FROM servicetbl WHERE id = ?" $
                       Only serviceId
  if status == ServiceStatus.ordered
  then do assigned <- isServiceAssignedToOperator serviceId
          if assigned -- назначена ли услуга на оператора
          then do
            _ <- carmaPostComment caseId "Партнёром произведён доезд до клиента"
            writeJSON servicePerformed
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
                     writeJSON servicePerformed
              _ -> do
                 writeJSON serviceActionNotfound

  else if status == ServiceStatus.ok
       then writeJSON serviceAlreadyPerformed
       else do
         _ <- carmaPostComment caseId "Партнёр приступил к оказанию услуги"
         writeJSON servicePerformed


statusServicePerformed :: AppHandler ()
statusServicePerformed = checkAuthCasePartner $ do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  comment   <- T.strip . TE.decodeUtf8 . fromMaybe "" <$> getParam "comment"

  when (T.null comment) $ error "empty comment"

  [(caseId :: Int, _status :: Int)] <- query
                       "SELECT parentid, status FROM servicetbl WHERE id = ?" $
                       Only serviceId
  _ <- carmaPostComment caseId $ T.unpack comment
  writeJSON servicePerformed


serviceMessage :: T.Text -> M.Map T.Text T.Text
serviceMessage message = M.fromList [ ( "status" :: T.Text
                                      , message
                                      )
                                    ]

-- Услуга выполнена
servicePerformed :: M.Map T.Text T.Text
servicePerformed = serviceMessage "service_performed"

-- Услуга уже выполнена
serviceAlreadyPerformed :: M.Map T.Text T.Text
serviceAlreadyPerformed = serviceMessage "service_already_performed"

-- Действие не найдено
serviceActionNotfound :: M.Map T.Text T.Text
serviceActionNotfound = serviceMessage "action_not_found"
