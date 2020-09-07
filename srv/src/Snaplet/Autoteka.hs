module Snaplet.Autoteka
  ( Autoteka
  , autotekaInit
  ) where


import           Prelude hiding ((.))
import           BasicPrelude
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Cfg

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.State (gets)

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Servant.Client as Servant

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth (AuthManager)
import           Snap.Snaplet.PostgresqlSimple (Postgres, execute, query)
import           Snaplet.Auth.Class
import           Snaplet.Auth.PGUsers (currentUserMetaId)

import           Database.PostgreSQL.Simple.SqlQQ
import           Carma.Utils.Snap (getIntParam, writeJSON, withLens)

import qualified Autoteka as Att



data Autoteka b = Autoteka
    { auth :: SnapletLens b (AuthManager b)
    , db :: SnapletLens b Postgres
    , clientEnv :: Servant.ClientEnv
    , session :: MVar Att.AccessToken
    }

instance HasPostgresAuth b (Autoteka b) where
  withAuth = withLens auth
  withAuthPg = withLens db


autotekaInit
  :: SnapletLens b (AuthManager b)
  -> SnapletLens b Postgres
  -> SnapletInit b (Autoteka b)
autotekaInit auth db = makeSnaplet "autoteka" "Car details searching" Nothing $ do
  cfg <- getSnapletUserConfig
  clientId <- liftIO $ Cfg.require cfg "client-id"
  clientSecret <- liftIO $ Cfg.require cfg "client-secret"

  env <- liftIO $ Att.mkClientEnv <$> newManager tlsManagerSettings
  ss <- liftIO $ startTokenRefreshThread env clientId clientSecret

  addRoutes
    [ ("/report/:caseId", method POST getReport)
    ]
  pure $ Autoteka auth db env ss


startTokenRefreshThread
  :: Servant.ClientEnv -> Text -> Text
  -> IO (MVar Att.AccessToken)
startTokenRefreshThread env clientId clientSecret = do
  let getToken = Servant.runClientM
        (Att.getToken clientId clientSecret)
        env
  -- FIXME: get timeout value from getToken response
  ss <- newEmptyMVar
  void $ forkIO $ forever
    $ getToken >>= \case
      Left _err -> threadDelay (60 * seconds) -- 1 min
      Right s' -> do
        emp <- isEmptyMVar ss
        if emp then putMVar ss s' else void $ swapMVar ss s'
        threadDelay (20 * 60 * seconds) -- 20 min
  return ss


runAPI
  :: (Att.AccessToken -> Servant.ClientM r)
  -> Handler b (Autoteka b) r
runAPI f = do
  ss <- gets session >>= liftIO . readMVar
  env <- gets clientEnv
  -- FIXME: catch err
  Right res <- liftIO $ Servant.runClientM (f ss) env
  return res


getReport :: Handler b (Autoteka b) ()
getReport = do
  Just caseId <- getIntParam "caseId"
  Just uid <- currentUserMetaId

  [[plateNum]] <- withLens db $ query
    [sql|
      select car_plateNum from casetbl where id = ?
    |]
    [caseId]

  [[reqId]] <- withLens db $ query
    [sql|
      insert into "AutotekaRequest"
        (caseId, userId, plateNumber)
        values (?, ?, ?)
        returning id
    |]
    (caseId, uid, plateNum :: Text)

  p <- runAPI $ \ss -> do
    pid <- Att.createPreview ss $ Att.RegNumber plateNum
    loop (1 * seconds)
      (\p -> Att.p_status p == "processing")
      (Att.getPreview ss pid)

  r <- runAPI $ \ss -> do
    rep <- Att.createReport ss (Att.p_previewId p)
    loop (1 * seconds)
      (\r -> Att.r_status r == "processing")
      (Att.getReport ss $ Att.r_reportId rep)

  let response = Aeson.toJSON r
  void $ withLens db $ execute
    [sql|
      insert into "AutotekaResponse"
        (requestId, caseId, response)
        values (?, ?, ?)
    |]
    (reqId :: Int, caseId, response)
  writeJSON $ Aeson.object
    [ ("response",  response)
    ]


seconds :: Int
seconds = 1000000


loop :: (MonadIO m, Aeson.ToJSON r) => Int -> (r -> Bool) -> m r -> m r
loop usec cond f = go
  where
    go = do
      res <- f
      if cond res
        then liftIO (threadDelay usec) >> go
        else return res
