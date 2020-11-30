-- |Main.hs
--
-- Proxy for:
--   - dadata.ru - suggestions on addresses, etc
--   - something else?
--
-- Copyright (C) ...

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}

module Main (main) where

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
import qualified Data.Map as Map -- to use its JSON encoding.
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)

import           System.Environment (getArgs)
import           System.Exit (exitFailure)

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
  let appCtx = token


      app =     serve (Proxy :: Proxy SearchRoute) (hoistServer (Proxy :: Proxy SearchRoute) withReader searchServer)
          where
            withReader :: ReaderT AppContext Handler a -> Handler a
            withReader r = runReaderT r appCtx

      warpSettings
          =   Warp.defaultSettings
            & Warp.setPort port
            & Warp.setHost (fromString host)

  Warp.runSettings warpSettings app


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

type AppContext = Text -- will expand on that later.

search
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadIO m
     )
  => SearchQuery -> m SearchResponse
search (SearchQuery query) = do
  token <- ask
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> token]
  r <- liftIO $ WReq.postWith opts
                                   "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address"
                                   (toJSON $ Map.fromList [("query" :: Text, query)])
  let body = r ^. WReq.responseBody
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
  token <- ask
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> token]
      params = Text.unpack $ "lon=" <> lon <> "&lat=" <> lat <> "&radius_meters=" <> rText
 
  --liftIO $ putStrLn $ show params
  r <- liftIO $ WReq.getWith opts
                                   ("https://suggestions.dadata.ru/suggestions/api/4_1/rs/geolocate/address?" <> params)
  let body = r ^. WReq.responseBody
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

