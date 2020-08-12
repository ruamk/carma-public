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
  Just caseId <- getIntParam "caseId"
  svcId <- getIntParam "serviceId"
  res <- query
    [sql|
      SELECT row_to_json(r.*)
        FROM (SELECT
            f.*,
            u.login,
            u.realname as "realName"
          FROM "CustomerFeedback" f
            JOIN usermetatbl u ON (f.userId = u.id)
          WHERE caseId = ? and (? is null or serviceId = ?)
        ) r
    |]
    (caseId :: Int, svcId, svcId :: Maybe Int)
  writeJSON (concat res :: [Aeson.Value])


data NewFeedback = NewFeedback
  { caseId    :: Int
  , serviceId :: Maybe Int
  , value     :: Int
  , comment   :: Text
  } deriving (Generic, Aeson.FromJSON)


newCustomerFeedback :: AppHandler ()
newCustomerFeedback = do
  NewFeedback{..} <- getJSONBody
  Just userId <- currentUserMetaId
  res <- query
    [sql|
      INSERT INTO "CustomerFeedback"
        (userId, caseId, serviceId, value, comment)
      VALUES
        (?, ?, ?, ?, ?)
      RETURNING id
    |]
    (userId, caseId, serviceId, value, comment)
  writeJSON (concat res :: [Int])
