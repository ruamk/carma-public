module AppHandlers.Services
    ( latestServices
    ) where

import           Data.Aeson (ToJSON, toJSON)
import           Data.Aeson.Types
import qualified Data.Map as Map
import           Data.Map ((!))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Time.LocalTime (ZonedTime)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics (Generic)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
                 ( query
                 , In (..), Only (..)
                 )

import           Data.Model
import           Application
import           AppHandlers.Users
import           AppHandlers.Util
import qualified Carma.Model.ServiceStatus as SS
import           Types


servicesLimit :: Int
servicesLimit = 1000


data CurrentServiceInfo = CurrentServiceInfo
    { cuCaseId :: Int
    , cuServiceId :: Int -- идентификатор услуги
    , cuServiceSerial :: Int -- номер услуги в списке услуг для заявки
    , _cuCallDate :: Maybe ZonedTime
    , _cuTypeOfService :: String
    , _cuStatus :: String
    , _cuAccordTime :: String
    , _cuMakeModel :: String
    , _cuBreakdownPlace :: String
    , _cuPayType :: String
    } deriving (Show ,Generic)

instance ToJSON CurrentServiceInfo where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

instance FromRow CurrentServiceInfo where
    fromRow = CurrentServiceInfo <$> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field
                                 <*> field


data ClosingServiceInfo = ClosingServiceInfo
    { clCaseId :: Int
    , clServiceId :: Int -- идентификатор услуги
    , clServiceSerial :: Int -- номер услиги в списке услуг для заявки
    , _clCallDate :: Maybe ZonedTime
    , _clTypeOfService :: String
    , _clMakeModel :: String
    , _clBreakdownPlace :: String
    , _clPayType :: String
    } deriving (Show, Generic)
    
instance ToJSON ClosingServiceInfo where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

instance FromRow ClosingServiceInfo where
    fromRow = ClosingServiceInfo <$> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field

-- | Handle get latest services
latestServices :: LatestServices -> AppHandler ()
latestServices serviceType = checkAuthCasePartner $ do
  user <- fromMaybe (error "No current user") <$> with auth currentUser
  let UserId uid = fromMaybe (error "no uid") $ userId user
  case serviceType of
    Current -> getLatestCurrentServices $ read $ T.unpack uid
    Closing -> getLatestClosingServices $ read $ T.unpack uid



getLatestCurrentServices :: Int -> AppHandler ()
getLatestCurrentServices uid = do
  let [ordered, delayed, inProgress] = map (\(Ident i) -> i)
                                       [SS.ordered, SS.delayed, SS.inProgress]

  rows :: [CurrentServiceInfo] <- query [sql|
    SELECT
        servicetbl.parentid
      , servicetbl.id
      , 0 as serviceSerial
      , times_expectedservicestart
      , st.label AS typeofservice
      , ss.label AS status
      , CASE
          WHEN servicetbl.status = ? THEN 'В работе'
          WHEN now() > times_expectedservicestart AND
               servicetbl.status IN ?
               THEN 'Опоздание'
          WHEN now() < times_expectedservicestart AND
               servicetbl.status IN ?
               -- returns: 'days hours minutes'
               THEN (extract(day from (times_expectedservicestart - now())::interval)
                    || ' ' ||
                    extract(hour from times_expectedservicestart - now())::text
                    || ' ' ||
                    to_char(extract(minute from times_expectedservicestart - now()), '00FM')
                    )

        END
      , coalesce(make.label || ' / ' ||
                 regexp_replace(model.label, '^([^/]*)/.*','\1'), '')::text
      , coalesce(casetbl.caseaddress_address, '')::text
      , coalesce(pt.label, '')::text
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" st ON st.id = type
    LEFT OUTER JOIN "ServiceStatus" ss ON ss.id = status
    LEFT OUTER JOIN "PaymentType" pt ON pt.id = paytype
    LEFT OUTER JOIN casetbl ON casetbl.id = parentid
    LEFT OUTER JOIN "CarMake" make ON make.id = casetbl.car_make
    LEFT OUTER JOIN "CarModel" model ON model.id = casetbl.car_model
    LEFT OUTER JOIN "CasePartner" cp
                 ON cp.partner = servicetbl.contractor_partnerid
    WHERE (servicetbl.status IN ?)
      AND cp.uid = ?
    ORDER BY times_expectedservicestart ASC
    LIMIT ?
  |] ( inProgress
     , In [ordered, delayed]
     , In [ordered, delayed]
     , In [ordered, delayed, inProgress]
     , uid
     , servicesLimit
     )

  serviceNums <- enumerateServices $ map cuCaseId rows

  writeJSON $ map (\m -> m { cuServiceSerial = serviceNums ! cuServiceId m} :: CurrentServiceInfo)
                  rows


getLatestClosingServices :: Int -> AppHandler ()
getLatestClosingServices uid = do
  rows :: [ClosingServiceInfo] <- query [sql|
    SELECT
        servicetbl.parentid
      , servicetbl.id
      , 0 as serviceSerial
      , createTime
      , st.label AS typeofservice
      , coalesce(make.label || ' / ' ||
                 regexp_replace(model.label, '^([^/]*)/.*','\1'), '')::text
      , coalesce(casetbl.caseaddress_address, '')::text
      , coalesce(pt.label, '')::text
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType"   st    ON st.id = type
    LEFT OUTER JOIN "ServiceStatus" ss    ON ss.id = status
    LEFT OUTER JOIN "PaymentType"   pt    ON pt.id = paytype
    LEFT OUTER JOIN casetbl               ON casetbl.id = parentid
    LEFT OUTER JOIN "CarMake"       make  ON make.id = casetbl.car_make
    LEFT OUTER JOIN "CarModel"      model ON model.id = casetbl.car_model
    LEFT OUTER JOIN "CasePartner"   cp    ON
         cp.partner = servicetbl.contractor_partnerid
    WHERE (servicetbl.status IN ?)
      AND cp.uid = ?
    ORDER BY createTime DESC
    LIMIT ?
  |] (In $ map (\(Ident i) -> i) [SS.ok] :: In [Int]
     , uid
     , servicesLimit
     )

  serviceNums <- enumerateServices $ map clCaseId rows

  writeJSON $ map (\m -> m { clServiceSerial = serviceNums ! clServiceId m} :: ClosingServiceInfo)
                  rows


enumerateServices :: [Int] -> AppHandler (Map.Map Int Int)
enumerateServices caseIds = fmap Map.fromList <$> query [sql|
    SELECT id, row_number() OVER (PARTITION BY parentid ORDER BY id)
    FROM servicetbl
    WHERE parentid in ?
  |] $ Only $ In caseIds
