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
      with events as
        (select caseId, serviceId,
          json_agg(json_strip_nulls(
            json_build_object(
              'ctime', r.ctime,
              'user', r.user,
              'operValue', r.response->'operValue',
              'techValue', r.response->'techValue',
              'comment', r.response->'comment'
            )
          ) order by r.ctime asc) as events
          from (select
              f.*,
              row_to_json(u.*) as "user"
            from "CustomerFeedback" f
              join lateral
                (select login, realName as "realName"
                  from usermetatbl u where u.id = f.userId) u
                on true
          ) r
          group by caseId, serviceId
        )
        select
          json_build_object(
            'caseId', caseId,
            'program', row_to_json(p.*),
            'service', row_to_json(s.*),
            'events', e.events
          )
          from events e
            join lateral
              (select p.label, p.id
                from "Program" p, casetbl c
                where c.id = e.caseId and p.id = c.program) p
              on true
            left join lateral
              (select s.id, st.label as "type", st.id as "typeId"
                from servicetbl s, "ServiceType" st
                where s.id = e.serviceId and st.id = s.type) s
              on true
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
        (userId, caseId, serviceId, response)
      VALUES
        (?, ?, ?, ?)
      RETURNING id
    |]
    (userId, caseId, serviceId, Aeson.toJSON response)
  writeJSON (concat res :: [Int])
