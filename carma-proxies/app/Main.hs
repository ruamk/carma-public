-- |Main.hs
--
-- Proxy for:
--   - dadata.ru - suggestions on addresses, etc
--   - something else?
--
-- Copyright (C) ...

{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import           Data.Aeson (ToJSON)
import           Data.Proxy
import           Data.Text (Text)

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO)

import           Servant
import qualified Network.Wai.Handler.Warp as Warp

import           Carma.Utils.Operators


-- Server routes
type AppRoutes
  =    -- Search coordinates by search query.
       -- Example: GET /search/ru-RU,ru/foobarbaz
       "search":> Capture "query" SearchQuery
                :> Get '[JSON] SearchResponse

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
  !(token :: Text)     <- Conf.require cfg "dadata_token"

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
     )
  => SearchQuery -> m SearchResponse
search _query = return $ SearchResponse ["nada", "nope", "нет"]

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

