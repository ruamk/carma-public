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

import           Servant
import qualified Network.Wai.Handler.Warp as Warp

import qualified Network.Wreq as WReq

type SearchRoute =
       -- Search coordinates by search query.
       -- Example: POST /search?query=bubuka%20zlobnaya
            ("search" :> QueryParam' '[Required] "query" SearchQuery
                 :> Post '[JSON] SearchResponse)
       -- Reverse search addresses by coordinates.
       -- Example: POSt /search?lon=65.10&lat=48.20
       :<|> ("revsearch" :> QueryParam' '[Required] "lon" SearchLon :> QueryParam' '[Required] "lat" SearchLat
                 :> Post '[JSON] SearchResponse)

main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "proxies.cfg"]

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
  => SearchLon -> SearchLat -> m SearchResponse
revSearch (SearchLon lon) (SearchLat lat)= do
  token <- ask
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> token]
      params = Text.unpack $ "lon=" <> lon <> "&lat=" <> lat
 
  liftIO $ putStrLn $ show params
  r <- liftIO $ WReq.getWith opts
                                   ("https://suggestions.dadata.ru/suggestions/api/4_1/rs/geolocate/address?" <> params)
  let body = r ^. WReq.responseBody
  liftIO $ putStrLn $ show body
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

{-
dadataAbbrevProxy :: AppHandler ()
dadataAbbrevProxy = logExceptions ("handler/abbrevproxy") $ do
  _token <- gets dadataToken
  Just js <- fmap decode $ readRequestBody 10000 :: AppHandler (Maybe Value)
  r <- liftIO $ NW.postWith (NW.defaults) "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address" js
  return ()

-}

