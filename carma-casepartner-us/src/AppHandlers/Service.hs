module AppHandlers.Service
    ( handleApiGetService
    , postComment
    , postPartnerDelay
    , serviceComments
    , serviceLocation
    , servicePerformed
    , statusInPlace
    , statusServiceClosed
    , statusServicePerformed
    ) where

import           Control.Monad                        (void, when)
import           Data.Aeson                           (ToJSON, Value (..),
                                                       genericToJSON, object,
                                                       toJSON)
import           Data.Aeson.Types                     (defaultOptions,
                                                       fieldLabelModifier)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.Read                       as TR
import           Data.Time.LocalTime                  (ZonedTime)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField,
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics                         (Generic)
import           Snap
import           Snap.Snaplet.PostgresqlSimple        (Only (..), execute,
                                                       query)

import           AppHandlers.Users
import           Application
import           Carma.Model                          (Ident (..), IdentI)
import           Carma.Model.Action                   (Action)
import qualified Carma.Model.ActionType               as ActionType
import qualified Carma.Model.PaymentType              as PaymentType
import qualified Carma.Model.ServiceStatus            as ServiceStatus
import qualified Carma.Model.Usermeta                 as Usermeta
import qualified Carma.Model.ServiceType              as ServiceType
import           Carma.Utils.Snap
import qualified Data.Model.Patch                     as Patch
import           Service.Util
import           Snaplet.Auth.PGUsers


type LoadingDifficulties = M.Map String (Maybe Bool)

instance FromField LoadingDifficulties where
    fromField = fromJSONField


data Payment = Payment
    { _partnerCost           :: Maybe Double
    , _partnerCostTranscript :: Maybe String
    , _checkCost             :: Maybe Double
    , _checkCostTranscript   :: Maybe String
    , _paidByClient          :: Maybe String
    } deriving (Show, Generic)

instance ToJSON Payment where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}


data ServiceDescription = ServiceDescription
    { _caseId               :: Int
    , _services             :: Int
    , _serviceType          :: String
    , _status               :: IdentI ServiceStatus.ServiceStatus
    , _statusLabel          :: String
    , _client               :: String
    , _clientPhone          :: String
    , _firstAddress         :: String
    , _lastAddress          :: String
    , _expectedServiceStart :: Maybe ZonedTime
    , _factServiceStart     :: Maybe ZonedTime
    , _factServiceEnd       :: Maybe ZonedTime
    , _makeModel            :: String
    , _plateNumber          :: String
    , _vin                  :: Maybe String
    , _payType              :: Maybe Int
    , _payment              :: Maybe Payment
    } deriving (Show, Generic)

instance ToJSON ServiceDescription where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}


data CaseComment = CaseComment
    { _datetime :: Maybe ZonedTime
    , _who      :: Maybe String
    , _json     :: Maybe Value
    } deriving (Show, Generic)

instance ToJSON CaseComment where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}


handleApiGetService :: AppHandler ()
handleApiGetService = checkAuthCasePartner $ do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  [( caseId, client, clientPhone, firstAddress, makeModel, plateNumber
   , vin)] <- query [sql|
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

  let [tech, towage, bikeTowage] =
          map (\(Ident i) -> i) [ServiceType.tech, ServiceType.towage, ServiceType.bikeTowage]

  [(expectedServiceStart, factServiceStart, factServiceEnd, serviceType
   , status, statusLabel, payType, partnerCost, partnerCostTranscript
   , checkCost, checkCostTranscript, paidByClient)] <- query [sql|
        SELECT
        servicetbl.times_expectedservicestart
      , servicetbl.times_factservicestart
      , servicetbl.times_factserviceend
      , CASE
          WHEN servicetbl.type = ?
               THEN st.label || ' - ' || tt.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - ' || ts.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - ' || btt.label
          ELSE
               st.label
        END AS typeofservice
      , servicetbl.status
      , ss.label
      , servicetbl.payType
      , servicetbl.payment_partnerCost
      , servicetbl.payment_partnerCostTranscript
      , servicetbl.payment_checkCost
      , servicetbl.payment_checkCostTranscript
      , servicetbl.payment_paidByClient
    FROM servicetbl
    LEFT OUTER JOIN techtbl             ON techtbl.id   = servicetbl.id
    LEFT OUTER JOIN towagetbl           ON towagetbl.id = servicetbl.id
    LEFT OUTER JOIN "BikeTowage"    bt  ON bt.id  = servicetbl.id
    LEFT OUTER JOIN "BikeTowType"   btt ON btt.id = bt.biketowtype
    LEFT OUTER JOIN "ServiceStatus" ss  ON ss.id  = servicetbl.status
    LEFT OUTER JOIN "ServiceType"   st  ON st.id  = servicetbl.type
    LEFT OUTER JOIN "TechType"      tt  ON tt.id  = techtbl.techtype
    LEFT OUTER JOIN "TowSort"       ts  ON ts.id  = towagetbl.towsort
    WHERE servicetbl.id = ?
    ORDER by servicetbl.id DESC
    LIMIT 1
  |] $ (tech, towage, bikeTowage, serviceId)

  lastAddress <- query [sql|
                  WITH service AS (
                    SELECT id, towaddress_address FROM towagetbl
                     UNION ALL
                    SELECT id, towaddress_address FROM "BikeTowage"
                  )
                  SELECT coalesce(towaddress_address, '')
                    FROM service
                   WHERE id = ?
                   LIMIT 1
                |] (Only serviceId)
                >>= \r -> return $ case r of
                                    (Only h:_) -> h
                                    _          -> ""

  let payment = if status `elem` [ServiceStatus.ok, ServiceStatus.closed]
                then Just $  Payment partnerCost partnerCostTranscript
                                     checkCost checkCostTranscript
                                     paidByClient
                else Nothing

  writeJSON $ ServiceDescription
               caseId serviceSerial serviceType
               status statusLabel
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               vin payType payment


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
  caseId <- fromMaybe (error "invalid case id") <$> getIntParam "caseId"
  comment <- T.strip . TE.decodeUtf8 . fromMaybe "" <$> getParam "comment"

  when (T.null comment) (error "empty comment")

  commentId <- carmaPostComment caseId $ T.unpack comment
  writeJSON [commentId]


-- | Return driver and client location to show on single map
-- No need for authorization, because it is for public resource!
serviceLocation :: AppHandler ()
serviceLocation = do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"

  [Only (clientCoords :: Maybe Text)] <- query [sql|
    SELECT caseAddress_coords
      FROM servicetbl
         , casetbl
     WHERE servicetbl.id = ?
       AND casetbl.id = servicetbl.parentId
  |] (Only serviceId)

  driverCoords :: [[Maybe Double]] <- query [sql|
    SELECT ST_X(ST_EndPoint(geometry(locations)))
         , ST_Y(ST_EndPoint(geometry(locations)))
      FROM driverServiceQueue
     WHERE serviceId = ?
  |] $ Only serviceId

  let coords lat lon = object [ ("latitude", lat)
                              , ("longitude", lon)
                              ]

      client = case clientCoords of
                 Just c  -> case map (TR.double . T.strip) $ T.split (== ',') c of
                             [Right (lon, ""), Right (lat, "")] ->
                               coords (toJSON lat) (toJSON lon)
                             _          -> coords Null Null
                 Nothing -> coords Null Null

      driver = case driverCoords of
                 [[Just lat, Just lon]] -> coords (toJSON lat) (toJSON lon)
                 _                      -> coords Null Null

  writeJSON [ object [ ("client", client)
                     , ("driver", driver)
                     ]
            ]


-- POST ../:service/partnerdelay
-- post params: comment - string
--              minutes - int
--              reason - int
postPartnerDelay :: AppHandler ()
postPartnerDelay = checkAuthCasePartner $ do
  Just user <- currentUserMeta
  let Just (Ident uid) = Patch.get user Usermeta.ident

  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  minutes   <- fromMaybe (error "invalid minutes") <$> getIntParam "minutes"
  reason    <- fromMaybe (error "invalid reason") <$> getIntParam "reason"
  comment <- (\case
              Just s  -> Just $ T.strip $ TE.decodeUtf8 s
              Nothing -> Nothing
            )
            <$> getParam "comment"

  partnerDelayId <- setStatusPartnerDelay serviceId uid minutes reason comment
  writeJSON [partnerDelayId]


-- | Сменить статус на "На месте"
-- Returns: {'status': 'service_performed'}
--          {'status': 'service_already_performed'}
statusInPlace :: AppHandler ()
statusInPlace = checkAuthCasePartner $ do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  setStatusInPlace serviceId >>= writeJSON


statusServicePerformed :: AppHandler ()
statusServicePerformed = checkAuthCasePartner $ do
  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"
  comment   <- T.strip . TE.decodeUtf8 . fromMaybe "" <$> getParam "comment"

  when (T.null comment) $ error "empty comment"

  setStatusPerformed serviceId (T.unpack comment) >>= writeJSON


statusServiceClosed :: AppHandler ()
statusServiceClosed = checkAuthCasePartner $ do
  Just user <- currentUserMeta
  let Just (Ident _uid) = Patch.get user Usermeta.ident

  serviceId <- fromMaybe (error "invalid service id") <$> getIntParam "serviceId"

  [Only serviceClosed] <- query [sql|
    SELECT EXISTS (
      SELECT id
        FROM actiontbl
       WHERE serviceId = ?
         AND type = ?
         AND result IS NOT NULL
    )
  |] (serviceId, ActionType.closeCase)

  when serviceClosed $ error $ "service " ++ show serviceId ++ " already closed"

  -- Услуга может быть закрыта, если проведена оценка оператором в карме
  -- то есть существует действие (action) с типом ActionType.checkEndOfService
  -- и результат действия определён.
  closeActionId :: [Only (IdentI Action)] <- query [sql|
    SELECT id
      FROM actiontbl
     WHERE serviceId = ?
       AND type = ?
       AND result IS NULL
  |] (serviceId, ActionType.closeCase)

  case closeActionId of
    [Only closeActionId'] -> closeService serviceId closeActionId'
    _ -> error $ "service " ++ show serviceId ++ " not checked"

  where
    closeService serviceId closeActionId = do
      partner <- getDoubleParam "partner"
      partnerCostTranscript <- (\case
                                Just s  -> Just $ T.strip $ TE.decodeUtf8 s
                                Nothing -> Nothing
                              ) <$> getParam "partnerTranscript"
      client <- getDoubleParam "client"

      result :: [Only (IdentI PaymentType.PaymentType)] <- query
        "SELECT payType FROM servicetbl WHERE id = ?"
        $ Only serviceId

      message <- case result of
        [] -> error "service id not found"
        (Only payType):_
            | payType == PaymentType.ruamc  -> do
                 case partner of
                   Just partnerCost -> do
                     when (partnerCost < 0.0) $ error "partner < 0.0"
                     void $ execute [sql|
                       UPDATE servicetbl
                          SET payment_partnerCost = ?
                            , payment_partnerCostTranscript = ?
                        WHERE id = ?
                     |] (partnerCost, partnerCostTranscript, serviceId)
                     setStatusClosed (Ident serviceId) closeActionId
                   Nothing -> error "partner not specified"

            | payType == PaymentType.client ||
              payType == PaymentType.refund -> do
                 case client of
                   Just clientCost -> do
                     when (clientCost < 0.0) $ error "client < 0.0"
                     void $ execute [sql|
                       UPDATE servicetbl
                          SET payment_paidByClient = ?
                        WHERE id = ?
                     |] (clientCost, serviceId)
                     setStatusClosed (Ident serviceId) closeActionId
                   Nothing -> error "client not specified"

            | payType == PaymentType.mixed  -> do
                 case (partner, client) of
                   (Just partnerCost, Just clientCost)
                       | partnerCost < 0.0 -> error "partner < 0.0"
                       | clientCost  < 0.0 -> error "client < 0.0"
                       | otherwise -> do
                           void $ execute [sql|
                             UPDATE servicetbl
                                SET payment_partnerCost = ?
                                  , payment_partnerCostTranscript = ?
                                  , payment_paidByClient = ?
                              WHERE id =?
                           |] ( partnerCost
                              , partnerCostTranscript
                              , clientCost
                              , serviceId
                              )
                           setStatusClosed (Ident serviceId) closeActionId
                   (Nothing, _) -> error "partner not specified"
                   (_, Nothing) -> error "client not specified"

            | otherwise -> error $ "unknown payType" ++ show payType

      writeJSON message
