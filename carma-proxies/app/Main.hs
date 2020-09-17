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
import           Data.Text.Encoding (encodeUtf8)

import           Servant
import qualified Network.Wai.Handler.Warp as Warp

import qualified Network.Wreq as WReq


-- Server routes
type AppRoutes
  =    -- Search coordinates by search query.
       -- Example: GET /search/ru-RU,ru/foobarbaz
       "search":> Capture "query" SearchQuery
                :> Post '[JSON] SearchResponse

{-
  :<|> -- Search addresses by coordinates
       -- Example: GET /reverse-search/ru-RU,ru/52.32,3.45
       "reverse-search" :> Capture "lang" Lang
                        :> Capture "coords" Coords
                        :> Get '[JSON] SearchByCoordsResponse
  :<|> -- /debug/...
       "debug" :> (    -- GET /debug/cached-queries
                       "cached-queries" :> Get '[JSON] [DebugCachedQuery]

                  :<|> -- GET /debug/cached-responses
                       "cached-responses" :> Get '[JSON] [DebugCachedResponse]

                  :<|> -- GET /debug/statistics
                       "statistics" :> Get '[JSON] [StatisticsDay]
                  )
-}


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "proxies.cfg"]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"
  !(token :: Text)     <- Conf.require cfg "dadata-token"

  let appCtx = token

      api = Proxy :: Proxy AppRoutes

      app = serve api $ hoistServer api withReader appServer
        where
          withReader :: ReaderT AppContext Handler a -> Handler a
          withReader r = runReaderT r appCtx

      warpSettings
        = Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)

  Warp.runSettings warpSettings app


appServer
  :: ( MonadReader AppContext m
     , MonadCatch m
     , MonadBaseControl IO m
     , MonadBase IO m
     , MonadIO m
     )
  => ServerT AppRoutes m
appServer =
  ( search
  )


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
  liftIO $ putStrLn $ "token: "++show token
  liftIO $ putStrLn $ "query: "++show query
  let opts = WReq.defaults & WReq.header "Authorization" .~ [encodeUtf8 $ fromString "Token " <> token]
  r <- liftIO $ WReq.postWith opts
                                   "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address"
                                   (toJSON $ Map.fromList [("query" :: Text, query)])
  liftIO $ putStrLn $ "dadata's response: "++show r
  let body = r ^. WReq.responseBody
  case fmap (\value -> fmap fromJSON $ (value ^? key "suggestions")) $ (decode body :: Maybe Value) of
    Just (Just (Success suggestions)) -> return $ SearchResponse suggestions
    Just (Just (Error err)) -> error $ "invalid json parsing: "++show err
    _ -> error "can't decode body"

newtype SearchQuery = SearchQuery Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData SearchQuery where
  parseQueryParam = Right . SearchQuery

newtype SearchResponse = SearchResponse [Text]
  deriving (Eq, Ord, Show)

deriving instance ToJSON SearchResponse

{-
dadataAbbrevProxy :: AppHandler ()
dadataAbbrevProxy = logExceptions ("handler/abbrevproxy") $ do
  _token <- gets dadataToken
  Just js <- fmap decode $ readRequestBody 10000 :: AppHandler (Maybe Value)
  r <- liftIO $ NW.postWith (NW.defaults) "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address" js
  return ()

-}

