{-# LANGUAGE FlexibleContexts, LambdaCase, QuasiQuotes #-}

module Carma.EraGlonass.Server.Helpers
     ( runSqlProtected
     ) where

import           Data.Text (Text)
import           Text.InterpolatedString.QM

import           Control.Monad ((>=>))
import           Control.Monad.Error.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Logger (LogSource)
import           Control.Exception (displayException)

import           Servant

import           Carma.Monad.LoggerBus.Class
import           Carma.EraGlonass.Instance.Persistent
                   ( DBAction
                   , MonadPersistentSql
                   )
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Helpers (runSqlInTime)


-- | Helps to automatically write to log about failed request to a database and
--   to throw 500 HTTP response with the same error message.
runSqlProtected
  ::
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadPersistentSql m
   , MonadError ServerError m
   )
  => LogSource
  -> Text -- ^ Fail message
  -> DBAction a
  -> m a

runSqlProtected logSrc errMsg =
  runSqlInTime >=> \case
    Right x -> pure x
    Left  e -> do
      let logMsg = [qmb| Database request is failed: {errMsg}
                         Exception: {displayException e} |]

      logErrorS logSrc [qm| {logMsg} |]
      throwError err500 { errBody = logMsg }
