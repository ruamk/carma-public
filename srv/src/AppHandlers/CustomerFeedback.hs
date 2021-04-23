{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AppHandlers.CustomerFeedback
    ( getCustomerFeedback
    , newCustomerFeedback
    )

where


import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Application (AppHandler)
import           Snap.Snaplet.PostgresqlSimple (query)
import           Snaplet.Auth.PGUsers (currentUserMetaId)

import           Carma.Utils.Snap (getJSONBody)
import           Util (getIntParam, writeJSON)

import           GHC.Generics (Generic)


getCustomerFeedback :: AppHandler ()
getCustomerFeedback = do
  caseId <- getIntParam "caseId"
  svcId <- getIntParam "serviceId"
  res <- query
    [sql|
      select jsn from "CustomerFeedback_view"
        where (? is null or caseId = ?) and (? is null or serviceId = ?)
        order by caseId desc, serviceId nulls first
        limit 100
    |]
    [caseId, caseId, svcId, svcId :: Maybe Int]
  writeJSON (concat res :: [Aeson.Value])


data Response  = Response
  { operValue :: Maybe Int
  , techValue :: Maybe Int
  , comment   :: Maybe Text
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

data NewFeedback = NewFeedback
  { caseId    :: Int
  , serviceId :: Maybe Int
  , response  :: Response
  } deriving (Generic, Aeson.FromJSON)


-- FIXME: check permissions
--  - type=resolved only if bo_qa
newCustomerFeedback :: AppHandler ()
newCustomerFeedback = do
  NewFeedback{..} <- getJSONBody
  Just userId <- currentUserMetaId
  res <- query
    [sql|
      INSERT INTO "CustomerFeedback"
        (eventType, userId, caseId, serviceId, data)
      VALUES
        ('FeedbackReceived', ?, ?, ?, ?)
      RETURNING id
    |]
    (userId, caseId, serviceId, Aeson.toJSON response)
  writeJSON (concat res :: [Int])
