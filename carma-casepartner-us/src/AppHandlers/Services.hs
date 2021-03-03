module AppHandlers.Services
    ( latestServices
    ) where


import           Data.Aeson                         (ToJSON, genericToJSON,
                                                     toJSON)
import           Data.Aeson.Types                   (defaultOptions,
                                                     fieldLabelModifier)
import           Data.Map                           ((!))
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import           Data.String                        (fromString)
import qualified Data.Text                          as T
import           Data.Time                          (Day)
import           Data.Time.Format                   (defaultTimeLocale,
                                                     formatTime)
import           Data.Time.LocalTime                (ZonedTime)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics                       (Generic)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple      (In (..), Only (..), Query,
                                                     query)

import           AppHandlers.Users
import           Application
import qualified Carma.Model.ServiceStatus          as SS
import qualified Carma.Model.ServiceType            as ST
import           Carma.Utils.Snap
import           Data.Model
import           Types


servicesLimit :: Int
servicesLimit = 1000

maxLimit :: Int
maxLimit = 100

defaultLimit :: Int
defaultLimit = 25

data CurrentServiceInfo = CurrentServiceInfo
    { cuCaseId          :: Int
    , cuServiceId       :: Int -- идентификатор услуги
    , cuServiceSerial   :: Int -- номер услуги в списке услуг для заявки
    , _cuCallDate       :: Maybe ZonedTime
    , _cuTypeOfService  :: Maybe String
    , _cuStatus         :: String
    , _cuAccordTime     :: String
    , _cuMakeModel      :: String
    , _cuBreakdownPlace :: String
    , _cuPayType        :: String
    } deriving (Show, Generic)

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
    { clCaseId          :: Int
    , clServiceId       :: Int -- идентификатор услуги
    , clServiceSerial   :: Int -- номер услиги в списке услуг для заявки
    , _clCallDate       :: Maybe ZonedTime
    , _clTypeOfService  :: Maybe String
    , _clMakeModel      :: String
    , _clBreakdownPlace :: String
    , _clPayType        :: String
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

data ServiceInfo = ServiceInfo
    { caseId          :: Int
    , serviceId       :: Int -- идентификатор услуги
    , serviceSerial   :: Int -- номер услиги в списке услуг для заявки
    , _callDate       :: Maybe ZonedTime
    , _typeOfService  :: Maybe String
    , _makeModel      :: String
    , _breakdownPlace :: String
    , _payType        :: String
    , _status         :: String 
    } deriving (Show, Generic)

instance ToJSON ServiceInfo where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

instance FromRow ServiceInfo where
    fromRow = ServiceInfo <$> field
                          <*> field
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
  let UserId i = fromMaybe (error "no uid") $ userId user
      uid = read $ T.unpack i

  case serviceType of
    All     -> getServices uid
    Current -> getLatestCurrentServices uid
    Closing -> getLatestClosingServices uid


getLatestCurrentServices :: Int -> AppHandler ()
getLatestCurrentServices uid = do
  let [ordered, delayed, inProgress] =
          map (\(Ident i) -> i) [SS.ordered, SS.delayed, SS.inProgress]
      [tech, towage, bikeTowage] =
          map (\(Ident i) -> i) [ST.tech, ST.towage, ST.bikeTowage]

  rows :: [CurrentServiceInfo] <- query [sql|
    SELECT
        servicetbl.parentid
      , servicetbl.id
      , 0 as serviceSerial
      , servicetbl.times_expectedservicestart
      , CASE
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || tt.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || ts.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || btt.label
          ELSE
               st.label
        END AS typeofservice
      , ss.label AS status
      , CASE
          WHEN servicetbl.status = ?
               THEN 'В работе'
          WHEN servicetbl.times_expectedservicestart is null
               THEN 'Ошибка'
          WHEN now() > servicetbl.times_expectedservicestart AND
               servicetbl.status IN ?
               THEN 'Опоздание'
          WHEN now() < servicetbl.times_expectedservicestart AND
               servicetbl.status IN ?
               -- returns: 'days hours minutes'
               THEN (extract(day from (servicetbl.times_expectedservicestart - now())::interval)
                    || ' ' ||
                    extract(hour from servicetbl.times_expectedservicestart - now())::text
                    || ' ' ||
                    to_char(extract(minute from servicetbl.times_expectedservicestart - now()), '00FM')
                    )
        END
      , coalesce(make.label || ' / ' ||
                 regexp_replace(model.label, '^([^/]*)/.*','\1'), '')::text
      , coalesce(casetbl.caseaddress_address, '')::text
      , coalesce(pt.label, '')::text
    FROM servicetbl
    LEFT OUTER JOIN casetbl               ON casetbl.id = parentid
    LEFT OUTER JOIN techtbl               ON techtbl.id = servicetbl.id
    LEFT OUTER JOIN towagetbl             ON towagetbl.id = servicetbl.id
    LEFT OUTER JOIN "BikeTowage"          ON "BikeTowage".id = servicetbl.id
    LEFT OUTER JOIN "BikeTowType"   btt   ON btt.id = "BikeTowage".biketowtype
    LEFT OUTER JOIN "CarMake"       make  ON make.id = casetbl.car_make
    LEFT OUTER JOIN "CarModel"      model ON model.id = casetbl.car_model
    LEFT OUTER JOIN "CasePartner"   cp    ON cp.partner = servicetbl.contractor_partnerid
    LEFT OUTER JOIN "PaymentType"   pt    ON pt.id = servicetbl.paytype
    LEFT OUTER JOIN "ServiceStatus" ss    ON ss.id = servicetbl.status
    LEFT OUTER JOIN "ServiceType"   st    ON st.id = servicetbl.type
    LEFT OUTER JOIN "TechType"      tt    ON tt.id = techtbl.techtype
    LEFT OUTER JOIN "TowSort"       ts    ON ts.id = towagetbl.towsort
    WHERE (servicetbl.status IN ?) AND cp.uid = ?
    ORDER BY times_expectedservicestart ASC
    LIMIT ?
  |] ( tech
     , towage
     , bikeTowage
     , inProgress
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
  let [tech, towage, bikeTowage] =
          map (\(Ident i) -> i) [ST.tech, ST.towage, ST.bikeTowage]
  rows :: [ClosingServiceInfo] <- query [sql|
    SELECT
        servicetbl.parentid
      , servicetbl.id
      , 0 as serviceSerial
      , servicetbl.times_expectedservicestart
      , CASE
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || tt.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || ts.label
          WHEN servicetbl.type = ?
               THEN st.label || ' - '::text || btt.label
          ELSE
               st.label
        END AS typeofservice
      , coalesce(make.label || ' / ' ||
                 regexp_replace(model.label, '^([^/]*)/.*','\1'), '')::text
      , coalesce(casetbl.caseaddress_address, '')::text
      , coalesce(pt.label, '')::text
    FROM servicetbl
    LEFT OUTER JOIN casetbl               ON casetbl.id = parentid
    LEFT OUTER JOIN techtbl               ON techtbl.id = servicetbl.id
    LEFT OUTER JOIN towagetbl             ON towagetbl.id = servicetbl.id
    LEFT OUTER JOIN "BikeTowage"          ON "BikeTowage".id = servicetbl.id
    LEFT OUTER JOIN "BikeTowType"   btt   ON btt.id = "BikeTowage".biketowtype
    LEFT OUTER JOIN "CarMake"       make  ON make.id = casetbl.car_make
    LEFT OUTER JOIN "CarModel"      model ON model.id = casetbl.car_model
    LEFT OUTER JOIN "CasePartner"   cp    ON cp.partner = servicetbl.contractor_partnerid
    LEFT OUTER JOIN "PaymentType"   pt    ON pt.id = servicetbl.paytype
    LEFT OUTER JOIN "ServiceStatus" ss    ON ss.id = servicetbl.status
    LEFT OUTER JOIN "ServiceType"   st    ON st.id = servicetbl.type
    LEFT OUTER JOIN "TechType"      tt    ON tt.id = techtbl.techtype
    LEFT OUTER JOIN "TowSort"       ts    ON ts.id = towagetbl.towsort
    WHERE (servicetbl.status IN ?) AND cp.uid = ?
    ORDER BY servicetbl.createTime DESC
    LIMIT ?
  |] ( tech
     , towage
     , bikeTowage
     , In $ map (\(Ident i) -> i) [SS.ok] :: In [Int]
     , uid
     , servicesLimit
     )

  serviceNums <- enumerateServices $ map clCaseId rows

  writeJSON $ map (\m -> m { clServiceSerial = serviceNums ! clServiceId m
                          } :: ClosingServiceInfo
                  ) rows


enumerateServices :: [Int] -> AppHandler (Map.Map Int Int)
enumerateServices caseIds = fmap Map.fromList <$> query [sql|
    SELECT id, row_number() OVER (PARTITION BY parentid ORDER BY id)
    FROM servicetbl
    WHERE parentid in ?
  |] $ Only $ In caseIds


inside :: Int -> Int -> Int -> Int
inside low high v
    | v > high  = high
    | v < low   = low
    | otherwise = v


getServices :: Int -> AppHandler ()
getServices uid = do
  offset <- inside 0 (maxBound :: Int) . fromMaybe 0
           <$> getIntParam "offset"
  limit <- inside 1 maxLimit . fromMaybe defaultLimit
          <$> getIntParam "limit"

  sId           <- getIntParam "serviceId"
  callDateStart <- getDateParam "callDateStart"
  callDateEnd   <- getDateParam "callDateEnd"

  let ds v = formatTime defaultTimeLocale "'%Y-%m-%d'" v
      de v = formatTime defaultTimeLocale "'%Y-%m-%d 23:59:59'" v
      condition :: String =
          case (sId, callDateStart :: Maybe Day, callDateEnd :: Maybe Day) of
               (Just i, _, _)      ->
                   "AND servicetbl.parentid = " ++ show i

               (_, Just s, Just e) ->
                   "AND servicetbl.times_expectedservicestart BETWEEN " ++ ds s ++
                   " AND " ++ de e

               (_, Just s, _)      ->
                   "AND servicetbl.times_expectedservicestart >= " ++ ds s

               (_, _, Just e)      ->
                   "AND servicetbl.times_expectedservicestart <= " ++ de e

               _                   -> ""

  let [tech, towage, bikeTowage] =
          map (\(Ident i) -> i) [ST.tech, ST.towage, ST.bikeTowage]

      sqlQuery = (fromString $ unwords
          [ "SELECT "
          , "  servicetbl.parentid"
          , ", servicetbl.id"
          , ", 0 as serviceSerial"
          , ", servicetbl.times_expectedservicestart"
          , ", CASE"
          , "    WHEN servicetbl.type = ?"
          , "         THEN st.label || ' - '::text || coalesce(tt.label,'')::text"
          , "    WHEN servicetbl.type = ?"
          , "         THEN st.label || ' - '::text || coalesce(ts.label,'')::text"
          , "    WHEN servicetbl.type = ?"
          , "         THEN st.label || ' - '::text || coalesce(btt.label,'')::text"
          , "    ELSE st.label"
          , "  END AS typeofservice"
          , ", coalesce(make.label || ' / ' || regexp_replace(model.label, '^([^/]*)/.*','\\1'), '')::text"
          , ", coalesce(casetbl.caseaddress_address, '')::text"
          , ", coalesce(pt.label, '')::text"
          , ", ss.label as status"
          , "FROM servicetbl"
          , "LEFT OUTER JOIN casetbl                 ON casetbl.id = parentid"
          , "LEFT OUTER JOIN techtbl                 ON techtbl.id = servicetbl.id"
          , "LEFT OUTER JOIN towagetbl               ON towagetbl.id = servicetbl.id"
          , "LEFT OUTER JOIN \"BikeTowage\"          ON \"BikeTowage\".id = servicetbl.id"
          , "LEFT OUTER JOIN \"BikeTowType\"   btt   ON btt.id = \"BikeTowage\".biketowtype"
          , "LEFT OUTER JOIN \"CarMake\"       make  ON make.id = casetbl.car_make"
          , "LEFT OUTER JOIN \"CarModel\"      model ON model.id = casetbl.car_model"
          , "LEFT OUTER JOIN \"CasePartner\"   cp    ON cp.partner = servicetbl.contractor_partnerid"
          , "LEFT OUTER JOIN \"PaymentType\"   pt    ON pt.id = servicetbl.paytype"
          , "LEFT OUTER JOIN \"ServiceStatus\" ss    ON ss.id = servicetbl.status"
          , "LEFT OUTER JOIN \"ServiceType\"   st    ON st.id = servicetbl.type"
          , "LEFT OUTER JOIN \"TechType\"      tt    ON tt.id = techtbl.techtype"
          , "LEFT OUTER JOIN \"TowSort\"       ts    ON ts.id = towagetbl.towsort"
          , "WHERE cp.uid = ? " ++ condition
          , "ORDER BY servicetbl.times_expectedservicestart DESC"
          , "LIMIT ? OFFSET ?"
          ]) :: Query

{-

data ServiceInfo = ServiceInfo
    { caseId          :: Int
    , serviceId       :: Int -- идентификатор услуги
    , serviceSerial   :: Int -- номер услиги в списке услуг для заявки
    , _callDate       :: Maybe ZonedTime
    , _typeOfService  :: Maybe String
    , _makeModel      :: String
    , _breakdownPlace :: String
    , _payType        :: String
    } deriving (Show, Generic)

-}


  rows :: [ServiceInfo] <- query sqlQuery ( tech, towage, bikeTowage
                                        , uid, limit, offset
                                        )

  serviceNums <- enumerateServices $ map caseId rows

  writeJSON $ map (\m -> m { serviceSerial = serviceNums ! serviceId m
                          } :: ServiceInfo
                  ) rows
