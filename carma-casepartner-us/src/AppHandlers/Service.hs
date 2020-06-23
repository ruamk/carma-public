module AppHandlers.Service
    where


import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON, Value)
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
import           CarmaApi
import           Carma.Model
import           Snaplet.Auth.PGUsers (currentUserMetaId)


type LoadingDifficulties = M.Map String (Maybe Bool)

instance FromField LoadingDifficulties where
    fromField = fromJSONField

data CaseDescription = CaseDescription
    { caseId :: Int
    , services :: Int
    , serviceType :: String
    , status :: Int
    , statusLabel :: String
    , client :: String
    , clientPhone :: String
    , firstAddress :: String
    , lastAddress :: String
    , expectedServiceStart :: Maybe ZonedTime
    , factServiceStart :: Maybe ZonedTime
    , factServiceEnd :: Maybe ZonedTime
    , makeModel :: String
    , plateNumber :: String
    , loadingDifficulties :: Maybe LoadingDifficulties
    , suburbanMilage :: String
    , vin :: Maybe String
    } deriving (Show, Generic)

instance ToJSON CaseDescription


data CaseComment = CaseComment
    { datetime :: Maybe ZonedTime
    , who :: Maybe String
    , json :: Maybe Value
    } deriving (Show, Generic)

instance ToJSON CaseComment
    
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
  |] $ (caseId, serviceId)

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

  writeJSON $ (CaseDescription
               caseId serviceSerial serviceType
               status statusLabel
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               loadingDifficulty
               suburbanMilage
               vin
              )


handleApiGetCaseComments :: AppHandler ()
handleApiGetCaseComments = checkAuthCasePartner $ do
  --user <- fromMaybe (error "No current user") <$> with auth currentUser
  --let UserId uid = fromMaybe (error "no uid") $ userId user
  Just (Ident uid) <- currentUserMetaId

  liftIO $ print $ "uid: " ++ show uid
  caseId <- fromMaybe (error "invalid case id") <$> getIntParam "caseId"
  limit <- fromMaybe 100 <$> getIntParam "limit"
  rows <- query [sql|
    SELECT datetime, who, json
    FROM "CaseHistory"
    WHERE caseId = ?
      AND (json::jsonb ?? 'serviceid' OR
           json->>'userid' = ?)
    ORDER BY datetime DESC
    LIMIT ?
  |] (caseId, show uid, limit)

  writeJSON $ map (\(datetime, who, json) -> CaseComment datetime who json)
                (rows :: [(Maybe ZonedTime, Maybe String, Maybe Value)])

-- POST ../:service/comment
postComment :: AppHandler ()
postComment = checkAuthCasePartner $ do
  -- user <- fromMaybe (error "No current user") <$> with auth currentUser
  caseId <- fromMaybe (error "invalid case id") <$> getIntParam "caseId"
  comment <- T.strip . TE.decodeUtf8 . fromMaybe "" <$> getParam "comment"

  when (T.null comment) (error "empty comment")

  cfg <- getSnapletUserConfig

  cookieName :: String <- liftIO $ (T.unpack . T.strip)
                               <$> lookupDefault "_session" cfg "carma.cookie"

  c <- getCookie $ BS.pack cookieName

  case c of
    Just c' -> do
      (Ident caseCommentId, _) <-
          addComment (cookieName ++ "=" ++ (BS.unpack $ cookieValue c')) caseId
                   $ T.unpack comment

      writeJSON [caseCommentId]
    Nothing -> error $ "no cookie named " ++ cookieName
