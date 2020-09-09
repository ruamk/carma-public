-- |Main.hs
--
-- Proxy for:
--   - dadata.ru - suggestions on addresses, etc
--   - something else?
--
-- Copyright (C) ...

module Main (main) where

import qualified Data.Configurator as Conf
import           Data.String (fromString)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (toJSON)
import           Data.Proxy
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Swagger (Swagger)
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import qualified Data.Time.Format as Time

import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Reader (MonadReader, asks, ReaderT, runReaderT)
import           Control.Monad.Catch (MonadThrow, MonadCatch, throwM, catchAll)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.IO.Class (MonadIO)

import           System.Directory (makeAbsolute)

import           Servant
import           Servant.Client hiding (Response)
import           Servant.Swagger (toSwagger)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Carma.Utils.Operators
import           Carma.Monad.LoggerBus.MonadLogger
import           Carma.Monad


-- Server routes
type AppRoutes
  =    -- Search coordinates by search query.
       -- Example: GET /search/ru-RU,ru/foobarbaz
       "search" :> Capture "lang" Lang
                :> Capture "query" SearchQuery
                :> Get '[JSON] [SearchByQueryResponse]

  :<|> -- Search addresses by coordinates
       -- Example: GET /reverse-search/ru-RU,ru/52.32,3.45
       "reverse-search" :> Capture "lang" Lang
                        :> Capture "coords" Coords
                        :> Get '[JSON] SearchByCoordsResponse
{-
  :<|> -- /debug/...
       "debug" :> (    -- GET /debug/cached-queries
                       "cached-queries" :> Get '[JSON] [DebugCachedQuery]

                  :<|> -- GET /debug/cached-responses
                       "cached-responses" :> Get '[JSON] [DebugCachedResponse]

                  :<|> -- GET /debug/statistics
                       "statistics" :> Get '[JSON] [StatisticsDay]
                  )
-}

type SwaggerHeaders a
   = Headers '[Header "Access-Control-Allow-Origin" String] a

-- Server routes + swagger debug route
type AppRoutesWithSwagger
  =    AppRoutes
  :<|> -- GET /debug/swagger.json
       "debug" :> "swagger.json" :> Get '[JSON] (SwaggerHeaders Swagger)


main :: IO ()
main = do
  cfg <- Conf.load [Conf.Required "app.cfg"]

  !(port :: Warp.Port) <- Conf.require cfg "port"
  !(host :: String)    <- Conf.lookupDefault "127.0.0.1" cfg "host"

  resCache            <- newIORefWithCounter mempty

  let appCtx = ()

      api = Proxy :: Proxy AppRoutesWithSwagger

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
  => ServerT AppRoutesWithSwagger m
appServer =
  ( search
    :<|> revSearch
    :<|> ( debugCachedQueries
           :<|> debugCachedResponses
           :<|> debugStatistics
         )
  )
  :<|> debugSwaggerAPI


-- Server routes handlers

type AppContext = () -- will expand on that later.

search
  :: ( MonadReader AppContext m
     , MonadCatch m
     )
  => Lang -> SearchQuery -> m [()]
search lang query =
  -- Writing statistics about any failure case

  throwUnexpectedResonse undefined

revSearch
  :: ( MonadReader AppContext m
     , MonadCatch m
     )
  => Lang -> Coords -> m SearchByCoordsResponse
revSearch lang coords@(Coords lon' lat') =

  throwUnexpectedResponse undefined

debugCachedQueries
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [DebugCachedQuery]
debugCachedQueries = do
  logInfo "Debugging cached queries..."

  (asks responsesCache >>= readIORefWithCounter)
    <&> -- Ordering by adding to cache time
        M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where reducer acc (k, (t, _)) =
          DebugCachedQuery k [qm| {debugFormatTime t} UTC |] : acc

debugCachedResponses
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [DebugCachedResponse]
debugCachedResponses = do
  logInfo "Debugging cached responses..."

  (asks responsesCache >>= readIORefWithCounter)
    <&> -- Ordering by adding to cache time
        M.assocs ? sortBy (compare `on` snd ? fst) ? foldl reducer []

  where
    reducer acc (k, (t, response'))
      = DebugCachedResponse
      { request_params = k
      , time           = [qm| {debugFormatTime t} UTC |]
      , response_type  = requestType response'
      , response       = rjson
      } : acc
      where rjson = case response' of
                         SearchByQueryResponse'  x -> toJSON x
                         SearchByCoordsResponse' x -> toJSON x

debugStatistics
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     , MonadIORefWithCounter m
     )
  => m [StatisticsDay]
debugStatistics = do
  logInfo "Reading collected statistics..."
  (asks statisticsData >>= readIORefWithCounter) <&> M.foldlWithKey reducer []

  where
    reducer acc k v = day k v : acc
    reqTypes = Nothing : fmap Just [minBound, maxBound] :: [Maybe RequestType]

    day k v
      = StatisticsDay
      { julian_day      = JulianDay k
      , iso_day         = ISODay k
      , by_request_type =
          reqTypes <&> \x -> StatisticsByRequestType
            { request_type = x
            , statistics   = HM.toList v <&> byReqType x & mconcat
            }
      }

    byReqType
      :: Maybe RequestType
      -> ((RequestType, StatisticResolve), Integer)
      -> RequestsStatistics
    byReqType reqTypeFilter ((reqType, resolve), c) =
      case reqTypeFilter of
           Nothing               -> x
           Just y | y == reqType -> x
                  | otherwise    -> mempty

      where x = mempty { total_requests = c }
                <> succeeded resolve c
                <> failed resolve c

    failed :: StatisticResolve -> Integer -> RequestsStatistics
    failed RequestIsFailed          c = mempty { total_failed = c }
    failed (RequestIsSucceeded _)   _ = mempty
    failed ResponseIsTakenFromCache _ = mempty

    succeeded :: StatisticResolve -> Integer -> RequestsStatistics
    succeeded RequestIsFailed _ = mempty
    succeeded (RequestIsSucceeded True) c
      = mempty
      { total_succeeded               = c
      , succeeded_real_requests       = c
      , succeeded_real_added_to_cache = c
      }
    succeeded (RequestIsSucceeded False) c
      = mempty
      { total_succeeded                   = c
      , succeeded_real_requests           = c
      , succeeded_real_not_added_to_cache = c
      }
    succeeded ResponseIsTakenFromCache c
      = mempty
      { total_succeeded            = c
      , succeeded_taken_from_cache = c
      }

-- Allowing cross-origin requests to be able to use online swagger-codegen.
debugSwaggerAPI
  :: Applicative m
  => m (Headers '[Header "Access-Control-Allow-Origin" String] Swagger)
debugSwaggerAPI = pure $ addHeader "*" $ toSwagger (Proxy :: Proxy AppRoutes)


-- Client requests to Nominatim

searchByQuery
  :: Maybe UserAgent
  -> Maybe NominatimAPIFormat
  -> Maybe Lang
  -> Maybe SearchQuery
  -> ClientM [SearchByQueryResponse]

reverseSearchByCoords
  :: Maybe UserAgent
  -> Maybe NominatimAPIFormat
  -> Maybe Lang
  -> Maybe NominatimLon
  -> Maybe NominatimLat
  -> ClientM SearchByCoordsResponse

(searchByQuery :<|> reverseSearchByCoords)
  = client (Proxy :: Proxy NominatimAPIRoutes)


-- Throwing `UnexpectedResponseResultException` with logging this error
throwUnexpectedResponse
  :: (MonadThrow m, MonadReader AppContext m, MonadLoggerBus m)
  => Response -> m a
throwUnexpectedResponse x = do
  logError [qm| Unexpected response result: {x}! |]
  throwM $ UnexpectedResponseResultException x


writeFailureToStatistics
  :: (MonadReader AppContext m, MonadClock m, MonadStatisticsWriter m)
  => RequestType -> m ()
writeFailureToStatistics reqType = do
  utcTime <- getCurrentTime
  writeStatistics utcTime reqType RequestIsFailed


debugFormatTime :: Time.FormatTime t => t -> String
debugFormatTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S"

{-
dadataAbbrevProxy :: AppHandler ()
dadataAbbrevProxy = logExceptions ("handler/abbrevproxy") $ do
  _token <- gets dadataToken
  Just js <- fmap decode $ readRequestBody 10000 :: AppHandler (Maybe Value)
  r <- liftIO $ NW.postWith (NW.defaults) "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address" js
  return ()

-}
