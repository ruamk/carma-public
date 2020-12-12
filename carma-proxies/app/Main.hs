-- |Main.hs
--
-- Proxy for:
--   - dadata.ru - suggestions on addresses, etc
--   - something else?
--
-- Copyright (C) ...

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO, liftIO)


import           Data.Aeson
import           Data.Aeson.Lens (key)
import qualified Data.Configurator as Conf
import           Data.Proxy
import qualified Data.List as L
import qualified Data.Map as Map -- to use its JSON encoding.
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO
import           System.IO.Unsafe

import           Servant
import qualified Network.Wai.Handler.Warp as Warp

import qualified Network.Wreq as WReq

type SearchRoute =
       -- Search coordinates by search query.
       -- Example: POST /search?query=bubuka%20zlobnaya
            ("geosearch" :> QueryParam' '[Required] "query" SearchQuery
                 :> Post '[JSON] SearchResponse)
       -- Reverse search addresses by coordinates.
       -- Example: POSt /search?lon=65.10&lat=48.20
       :<|> ("revgeosearch" :> QueryParam' '[Required] "lon" SearchLon
                            :> QueryParam' '[Required] "lat" SearchLat
                            :> QueryParam' '[Required] "radius" SearchRadius
                 :> Post '[JSON] SearchResponse)

main :: IO ()
main = do
  cfgName <- do
    args <- getArgs
    case args of
      ("-h":_) -> do
        putStrLn $ unlines
                     [  "usage: carma-proxies [config-file-name|-h]"
                     , ""
                     , "If omitted, config-file-name takes value proxies.cfg."
                     , ""
                     , "-h will print this usage text"
                     ]
        exitFailure
      [fn] -> return fn
      [] -> return "proxies.cfg"
      _ -> do
        putStrLn $ "invalid usage; use 'carma-proxies -h' for usage"
        exitFailure
  cfg <- Conf.load [Conf.Required cfgName]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"
  !(token :: Text)     <- Conf.require cfg "dadata-token"
  (logLevel :: String) <- Conf.lookupDefault "start-stop" cfg "log-level"
  logRequests <- case logLevel of
    "start-stop" -> return False
    "debug"      -> return True
    _            -> do
                      putStrLn $ "Invalid logging level "++show logLevel++" (field log-level in configuration file)"
                      putStrLn "Allowed values: start-stop and debug."
                      exitFailure
  addressSearch <- Conf.require cfg "dadata-suggestions"
  coordSearch   <- Conf.require cfg "dadata-geosuggestions"
  let appCtx = AppContext token (if logRequests then priorityDebug else priorityInfo) addressSearch coordSearch


      app =     serve (Proxy :: Proxy SearchRoute) (hoistServer (Proxy :: Proxy SearchRoute) withReader searchServer)
          where
            withReader :: ReaderT AppContext Handler a -> Handler a
            withReader r = runReaderT r appCtx

      warpSettings
          =   Warp.defaultSettings
            & Warp.setPort port
            & Warp.setHost (fromString host)

  logMsg priorityInfo $ "carma-proxies start: "++show host++", port "++show port
  Warp.runSettings warpSettings app
  logMsg priorityInfo "carma-proxies stop"

type Priority = Int

priorityInfo, priorityDebug :: Priority
priorityInfo  = 5
priorityDebug = 6

-- | Write a message to stderr.
-- We use strict text here to force message evaluation in the calling thread.
logMsg :: MonadIO m => Priority -> String -> m ()
logMsg p msg = liftIO $ do
  -- Prepend each line with a numeric representation of the specified priority.
  -- This will be parsed by systemd (see 'man sd_journal_print' for more info).
  let prefix = "<" <> show p <> ">"
  let msg' = L.intercalate "\n"
        $ map (prefix <>)
        $ lines $ msg
  -- Write directly to stderr if log thread has not started.
  withMVar logLock $ \_ ->
    hPutStrLn stderr msg'

logDebug :: MonadIO m => Priority -> String -> m ()
logDebug p msg
  | p /= priorityDebug = return ()
  | otherwise = logMsg p msg

logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()
{-# NOINLINE logLock #-}


searchServer
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadBaseControl IO m
     , MonadBase IO m
     , MonadIO m
     )
  => ServerT SearchRoute m
searchServer = search :<|> revSearch

-- Server routes handlers

data AppContext = AppContext
  { appToken               :: Text
  , appLogLevel            :: Int
  , appAddressSearchURL    :: String
  , appCoordinateSearchURL :: String
  }

search
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadIO m
     )
  => SearchQuery -> m SearchResponse
search (SearchQuery query) = do
  AppContext {..} <- ask
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> appToken]
  r <- liftIO $ WReq.postWith opts appAddressSearchURL (toJSON $ Map.fromList [("query" :: Text, query)])
  let body = r ^. WReq.responseBody
  logDebug appLogLevel $ unlines
                         [ "search query "++show query
                         , "response: "++show r
                         ]
  case fmap (\value -> fmap fromJSON $ (value ^? key "suggestions")) $ (decode body :: Maybe Value) of
    Just (Just (Success suggestions)) -> return $ SearchResponse suggestions
    Just (Just (Error err)) -> error $ "invalid json parsing: "++show err
    _ -> error "can't decode body"

newtype SearchQuery = SearchQuery Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData SearchQuery where
  parseQueryParam = Right . SearchQuery

newtype SearchResponse = SearchResponse Value
  deriving (Show)

deriving instance ToJSON SearchResponse

revSearch
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadIO m
     )
  => SearchLon -> SearchLat -> SearchRadius -> m SearchResponse
revSearch (SearchLon lon) (SearchLat lat) (SearchRadius rText) = do
  AppContext {..} <- ask
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> appToken]
      params = Text.unpack $ "lon=" <> lon <> "&lat=" <> lat <> "&radius_meters=" <> rText
 
  --liftIO $ putStrLn $ show params
  r <- liftIO $ WReq.getWith opts (appCoordinateSearchURL <> params)
  let body = r ^. WReq.responseBody
  logDebug appLogLevel $ unlines
                         [ "search lon "++show lon++", lat "++show lat++", radius "++show rText
                         , "response: "++show r
                         ]
  --liftIO $ putStrLn $ show body
  case fmap (\value -> fmap fromJSON $ (value ^? key "suggestions")) $ (decode body :: Maybe Value) of
    Just (Just (Success suggestions)) -> return $ SearchResponse suggestions
    Just (Just (Error err)) -> error $ "invalid json parsing: "++show err
    _ -> error "can't decode body"

newtype SearchLon = SearchLon Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData SearchLon where
  parseQueryParam = Right . SearchLon

newtype SearchLat = SearchLat Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData SearchLat where
  parseQueryParam = Right . SearchLat

newtype SearchRadius = SearchRadius Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData SearchRadius where
  parseQueryParam = Right . SearchRadius

