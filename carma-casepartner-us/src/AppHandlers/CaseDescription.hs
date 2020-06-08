module AppHandlers.CaseDescription
    where


import           Data.Aeson (ToJSON, Value)
import qualified          Data.Map as M
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Data.Time.LocalTime (ZonedTime)
import           Database.PostgreSQL.Simple.FromField
                 (FromField, fromField, fromJSONField)
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple
                 ( query
                 , Only (..)
                 )

import           Application
import           AppHandlers.Util


type LoadingDifficulties = M.Map String (Maybe Bool)

instance FromField LoadingDifficulties where
    fromField = fromJSONField

data CaseDescription = CaseDescription
    { caseId :: Int
    , services :: Int
    , serviceType :: String
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

  [(expectedServiceStart, factServiceStart, factServiceEnd, serviceType)] <- query [sql|
    SELECT
        times_expectedservicestart
      , times_factservicestart
      , times_factserviceend
      , "ServiceType".label
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" ON servicetbl.type = "ServiceType".id
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
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               loadingDifficulty
               suburbanMilage
               vin
              )

handleApiGetCaseComments :: AppHandler ()
handleApiGetCaseComments = do
  caseId <- fromMaybe (error "invalid case id") <$>
              getIntParam "caseId"
  limit <- fromMaybe 100 <$> getIntParam "limit"
  rows <- query [sql|
    SELECT datetime, who, json
    FROM "CaseHistory"
    WHERE caseId = ?
    ORDER BY datetime DESC
    LIMIT ? 
  |] (caseId, limit)

  writeJSON $ map (\(datetime, who, json) ->
                       CaseComment datetime who json
                  ) (rows :: [(Maybe ZonedTime, Maybe String, Maybe Value)])


