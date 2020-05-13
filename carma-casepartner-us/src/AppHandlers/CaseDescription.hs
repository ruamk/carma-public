{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-- # OPTIONS_GHC -Wno-unused-top-binds #-}
module AppHandlers.CaseDescription
    where


import           Data.Aeson (ToJSON)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Data.Time.LocalTime (ZonedTime)
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple
                 ( query
                 , Only (..)
                 )

import           Application
import           AppHandlers.Util


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
    , loadingDifficulty :: String
    , suburbanMilage :: String
    } deriving (Show, Generic)

instance ToJSON CaseDescription


handleApiGetCase :: AppHandler ()
handleApiGetCase = do
  caseId <- fromMaybe (error "invalid case id") <$> getIntParam "caseId"
  [(client, clientPhone, firstAddress, makeModel, plateNumber)] <-
      query [sql|
              SELECT
                  contact_name
                , contact_phone1
                , caseaddress_address
                , "CarMake".label || ' / ' || regexp_replace("CarModel".label, '^([^/]*)/.*','\1')
                , car_platenum
              FROM casetbl
              LEFT OUTER JOIN "CarMake" ON "CarMake".id = car_make
              LEFT OUTER JOIN "CarModel" ON "CarModel".id = car_model
              WHERE casetbl.id = ?
  |] $ Only caseId

  [Only serviceCounter] <- query
    "SELECT count(*) FROM servicetbl WHERE parentid = ?"
    $ Only caseId

  [(expectedServiceStart, factServiceStart, factServiceEnd, serviceType)] <- query [sql|
      SELECT times_expectedservicestart
           , times_factservicestart
           , times_factserviceend
           , "ServiceType".label
      FROM servicetbl
      LEFT JOIN "ServiceType" ON servicetbl.type = "ServiceType".id
      WHERE servicetbl.parentid = ?
      ORDER by servicetbl.id DESC
      LIMIT 1
    |] $ Only caseId

  r1 <- query [sql|
    SELECT coalesce(towaddress_address, '')
         , coalesce(suburbanmilage::text, '')
         , coalesce(flags::text, '')
    FROM allservicesview
    WHERE parentid = ?
    LIMIT 1
  |] $ Only caseId
  let (lastAddress, suburbanMilage, loadingDifficulty) = if length r1 == 1
                                                         then head r1
                                                         else ("","", "")

  writeJSON $ (CaseDescription
               caseId serviceCounter serviceType
               client clientPhone
               firstAddress lastAddress
               expectedServiceStart factServiceStart factServiceEnd
               makeModel plateNumber
               loadingDifficulty
               suburbanMilage
              )
