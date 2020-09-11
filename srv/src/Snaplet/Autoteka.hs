module Snaplet.Autoteka
  ( Autoteka
  , autotekaInit
  ) where


import           Prelude hiding ((.))
import           BasicPrelude
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Cfg
import qualified Data.Text as T

import           Control.Exception.Lifted (try)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.State (gets)

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Servant.Client as Servant

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth (AuthManager)
import           Snap.Snaplet.PostgresqlSimple (Postgres, query)
import           Snaplet.Auth.Class
import           Snaplet.Auth.PGUsers (currentUserMetaId)

import           Database.PostgreSQL.Simple.SqlQQ
import           Carma.Utils.Snap (getIntParam, writeJSON, withLens)

import qualified Autoteka as Att

import           Util (syslogJSON, logExceptions, Priority(Info, Error))



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
    [("/report/:caseId", method POST getReport)]
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
      Left err -> do
        syslogJSON Error "Autoteka"
          [("msg", "Token refresh failed")
          ,("err", toJsonStr err)
          ]
        threadDelay (60 * seconds) -- 1 min
      Right s' -> do
        syslogJSON Info "Autoteka" [("msg", "Token refresh ok")]
        emp <- isEmptyMVar ss
        if emp then putMVar ss s' else void $ swapMVar ss s'
        threadDelay (20 * 60 * seconds) -- 20 min
  return ss



getReport :: Handler b (Autoteka b) ()
getReport = do
  Just caseId <- getIntParam "caseId"
  Just uid <- currentUserMetaId
  syslogJSON Info "Autoteka" [("caseId", toJsonStr caseId)]

  [(reqId, plateNum)] <- withLens db $ query
    [sql|
      insert into "AutotekaRequest"
        (caseId, userId, plateNumber)
        select id, ?, car_plateNum from casetbl where id = ?
        returning id, plateNumber
    |]
    (uid, caseId)

  report <- try
    $ logExceptions "Autoteka"
    $ getReport' plateNum

  let (rep, err) = case report of
        Right res -> (Just res, Nothing)
        Left ex ->
          ( Nothing
          , Just $ Aeson.object
            [("msg", toJsonStr (ex :: SomeException))]
          )

  [[response]] <- withLens db $ query
    [sql|
      update "AutotekaRequest" r
        set report = ?, error = ?, mtime = now()
        where id = ?
        returning row_to_json(r.*)
    |]
    (rep, err, reqId :: Int)
  writeJSON (response :: Aeson.Value)


-- This may take some time to execute (up to several minutes) and we expect that
-- session `ss` can't suddenly expire during the execution. To ensure this we
-- refresh session long before it expires.
getReport' :: Text -> Handler b (Autoteka b) Aeson.Value
getReport' plateNum = runAPI $ \ss -> do
  pid <- Att.createPreview ss $ Att.RegNumber plateNum
  syslogJSON Info "Autoteka" [("previewId", toJsonStr pid)]

  p <- loop (1 * seconds)
    (not . isFinal . Att.p_status)
    (Att.getPreview ss pid)

  syslogJSON Info "Autoteka"
    [("preview", Aeson.toJSON p)]

  rep <- Att.createReport ss (Att.p_previewId p)
  syslogJSON Info "Autoteka" [("reportId", toJsonStr rep)]

  r <- loop (1 * seconds)
    (not . isFinal . Att.r_status)
    (Att.getReport ss $ Att.r_reportId rep)
  return $ Aeson.toJSON r


isFinal :: Text -> Bool
isFinal s = s /= "processing" && s /= "wait"

toJsonStr :: Show s => s -> Aeson.Value
toJsonStr = Aeson.String . T.pack .show


runAPI
  :: (Att.AccessToken -> Servant.ClientM r)
  -> Handler b (Autoteka b) r
runAPI f = do
  ss <- gets session >>= liftIO . readMVar
  env <- gets clientEnv
  Right res <- liftIO $ Servant.runClientM (f ss) env
  return res


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
