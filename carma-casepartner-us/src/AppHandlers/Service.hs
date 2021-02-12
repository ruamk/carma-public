module AppHandlers.Service
    ( handleApiGetService
    , postComment
    , postPartnerDelay
    , serviceComments
    , serviceLocation
    , servicePerformed
    , statusInPlace
    , statusServicePerformed
    ) where

import           Control.Monad                        (when)
--import           Control.Monad.IO.Class               (liftIO)
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
import           Snap.Snaplet.PostgresqlSimple        (Only (..), query)

import           AppHandlers.Users
import           AppHandlers.Util
import           Application
import           Carma.Model
import qualified Carma.Model.Usermeta                 as Usermeta
import qualified Data.Model.Patch                     as Patch
import           Service.Util
import           Snaplet.Auth.PGUsers


type LoadingDifficulties = M.Map String (Maybe Bool)

instance FromField LoadingDifficulties where
    fromField = fromJSONField


data ServiceDescription = ServiceDescription
    { _caseId               :: Int
    , _services             :: Int
    , _serviceType          :: String
    , _status               :: Int
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

  [(expectedServiceStart, factServiceStart, factServiceEnd, serviceType
   , status, statusLabel, payType)] <- query [sql|
    SELECT
        times_expectedservicestart
      , times_factservicestart
      , times_factserviceend
      , "ServiceType".label
      , status
      , "ServiceStatus".label
      , payType
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" ON servicetbl.type = "ServiceType".id
    LEFT OUTER JOIN "ServiceStatus" ON servicetbl.status = "ServiceStatus".id
    WHERE servicetbl.id = ?
    ORDER by servicetbl.id DESC
    LIMIT 1
  |] $ Only serviceId

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

  writeJSON $ ServiceDescription
               caseId serviceSerial serviceType
               status statusLabel
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               vin payType


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
