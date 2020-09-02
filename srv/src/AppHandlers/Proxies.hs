{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}

{-|

Rukovoditel Koll Centra screen.

-}

module AppHandlers.Proxies
    (
      postProxyHandler
    )

where

import           BasicPrelude                       hiding ( intercalate
                                                           , groupBy
                                                           )
import qualified BasicPrelude                       as L ( intercalate
                                                         , groupBy
                                                         )

import           Control.Monad.State.Class
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Aeson
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Lazy               as LB
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time
import           Data.Semigroup

import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.ToField   as PS
import qualified Database.PostgreSQL.Simple.FromField as PS
import qualified Snap.Snaplet.PostgresqlSimple        as PS
import           Snap

import           Data.Model
import qualified Data.Model.Sql                     as Sql
import           Data.Model.Utils.LegacyModel

import qualified Carma.Model.ActionResult           as AResult
import qualified Carma.Model.ActionType             as AType
import           Carma.Model.City                   (City)
import qualified Carma.Model.ConsultationType       as CT
import           Carma.Model.Program                (Program)
import qualified Carma.Model.Satisfaction           as Satis
import qualified Carma.Model.ServiceStatus          as SS
import qualified Carma.Model.ServiceType            as ST

import           Application
import           Util

type Proxy m = (PS.HasPostgres m, MonadBaseControl IO m, MonadFail m)

postProxyHandler :: String -> AppHandler ()
psotProxyHandler name = logExceptions ("handler/postproxy/"++name) $ do
  proxiesConf <- gets proxiesCfg
  flt' <- mkRkcFilter
  info <- rkc flt'
  writeJSON info

