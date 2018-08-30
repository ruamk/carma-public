{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Carma.EraGlonass.Logger where

import           Text.InterpolatedString.QM
import           Data.ByteString (ByteString)

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Reader (ReaderT, ask, asks)
import           Control.Monad.Logger

import           System.Log.FastLogger (fromLogStr)

import           Carma.EraGlonass.Logger.LoggerForward
import           Carma.EraGlonass.Types (AppContext (loggerBus))
import           Carma.Monad.LoggerBus.Helpers (formatTime)
import           Carma.Monad.LoggerBus.Types (LogMessage (..))
import           Carma.Monad.LoggerBus
import           Carma.Monad.Thread
import           Carma.Monad.Clock
import           Carma.Monad.MVar


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus (ReaderT AppContext m)
         where

  logInfo  msg = asks loggerBus >>= lift . flip logInfoImpl  msg
  logError msg = asks loggerBus >>= lift . flip logErrorImpl msg
  readLog      = asks loggerBus >>= lift . readLogImpl


-- | When @AppContext@ isn't constructed yet but you need the logger bus.
instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLoggerBus (ReaderT (MVar LogMessage) m)
         where

  logInfo  msg = ask >>= lift . flip logInfoImpl  msg
  logError msg = ask >>= lift . flip logErrorImpl msg
  readLog      = ask >>= lift . readLogImpl


instance ( Monad m
         , MonadMVar m
         , MonadClock m
         , MonadThread m
         ) => MonadLogger (LoggerForward m) where

  monadLoggerLog loc src lvl msg = do
    !utc <- lift getCurrentTime
    loggerBus' <- askLoggerForward

    void $ lift $ fork $ -- Forking for non-blocking writing to MVar
      putMVar loggerBus' $
        LogForward loc src lvl $
          toLogStr ([qms| [{formatTime utc} UTC]
                          {fromLogStr $ toLogStr msg} |] :: ByteString)