{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module AppHandlers.CaseInfo
    where

import           Data.Aeson (ToJSON)
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

import           Application
import           AppHandlers.Users
import           AppHandlers.Util
import qualified Carma.Model.ServiceStatus as SS
import           Data.Model

import           Types


casesLimit :: Int
casesLimit = 1000


data CurrentCaseInfo = CurrentCaseInfo
    { cuCaseId :: Int
    , cuServices :: Int
    , cuCallDate :: Maybe ZonedTime
    , cuTypeOfService :: String
    , cuStatus :: String
    , cuAccordTime :: String
    , cuMakeModel :: String
    , cuBreakdownPlace :: String
    , cuPayType :: String
    } deriving (Show ,Generic)

instance ToJSON CurrentCaseInfo

instance FromRow CurrentCaseInfo where
    fromRow = CurrentCaseInfo <$> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field


data ClosingCaseInfo = ClosingCaseInfo
    { clCaseId :: Int
    , clServices :: Int
    , clCallDate :: Maybe ZonedTime
    , clTypeOfService :: String
    , clMakeModel :: String
    , clBreakdownPlace :: String
    , clPayType :: String
    } deriving (Show, Generic)
    
instance ToJSON ClosingCaseInfo

instance FromRow ClosingCaseInfo where
    fromRow = ClosingCaseInfo <$> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field
                              <*> field

-- | Handle get latest cases
handleApiGetLatestCases :: LatestCases -> AppHandler ()
handleApiGetLatestCases caseType = checkAuthCasePartner $ do
  user <- fromMaybe (error "No current user") <$> with auth currentUser
  let UserId uid = fromMaybe (error "no uid") $ userId user
  case caseType of
    Current -> getLatestCurrentCases $ read $ T.unpack uid
    Closing -> getLatestClosingCases $ read $ T.unpack uid



getLatestCurrentCases :: Int -> AppHandler ()
getLatestCurrentCases uid = do
  let [ordered, delayed, inProgress] = map (\(Ident i) -> i)
                                       [SS.ordered, SS.delayed, SS.inProgress]

  rows :: [CurrentCaseInfo] <- query [sql|
    SELECT
        servicetbl.parentid
      , servicetbl.id AS services
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
     , casesLimit
     )

  serviceNums <- enumerateServices $ map cuCaseId rows

  writeJSON $ map (\m -> m { cuServices = serviceNums ! cuServices m} :: CurrentCaseInfo)
                  rows


getLatestClosingCases :: Int -> AppHandler ()
getLatestClosingCases uid = do
  rows :: [ClosingCaseInfo] <- query [sql|
    SELECT servicetbl.parentid
         , servicetbl.id AS services
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
     , casesLimit
     )

  serviceNums <- enumerateServices $ map clCaseId rows

  writeJSON $ map (\m -> m { clServices = serviceNums ! clServices m} :: ClosingCaseInfo)
                  rows


enumerateServices :: [Int] -> AppHandler (Map.Map Int Int)
enumerateServices caseIds = fmap Map.fromList <$> query [sql|
    SELECT id, row_number() OVER (PARTITION BY parentid ORDER BY id)
    FROM servicetbl
    WHERE parentid in ?
  |] $ Only $ In caseIds
