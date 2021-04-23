
module Main where

import           Control.Monad (when, join)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as Config
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Default.Class
import qualified System.Environment as Env
import           System.IO (hPutStrLn, stderr)
import           Text.InterpolatedString.QM (qm, qn)
import           GHC.Generics

import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types (notFound404, internalServerError500)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai (Middleware)
import           Web.Scotty


data AppContext = AppContext
  { pgPool :: Pool PG.Connection
  , frontDir :: FilePath
  }


data Feedback = Feedback
  { operValue :: Maybe Int
  , techValue :: Maybe Int
  , comment   :: Maybe Text
  }
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)


main :: IO ()
main = Env.getArgs >>= \case
  [configPath] -> main' configPath
  _ -> do
    prog <- Env.getProgName
    hPutStrLn stderr [qm| Usage: {prog} <config.conf> |]


main' :: FilePath -> IO ()
main' configPath = do
  conf <- Config.load [Config.Required configPath]
  pgUri <- Config.require conf "pg.uri"
  httpPort <- Config.require conf "http.port"

  cxt <- AppContext
    <$> createPool (PG.connectPostgreSQL pgUri) PG.close
      1   -- number of distinct sub-pools
      200 -- seconds unused resource kept open
      5   -- maximum number of resources to keep open
    <*> Config.require conf "frontend.dir"

  logger <- mkRequestLogger $ def
    { destination = Handle stderr
    , outputFormat = Apache FromFallback
    }
  scotty httpPort $ server logger cxt


server :: Middleware -> AppContext -> ScottyM ()
server mw AppContext{..} = do
  middleware mw
  get "/api/:key" $ do
    key <- param "key"
    res <- liftIO $ withResource pgPool $ \c ->
      PG.query c
        [qn| select customer_feedback_by_urlKey(?) |]
        [key :: Text]
    case res of
      [[x]] -> json (x :: Aeson.Value)
      [] -> raiseStatus notFound404 "Invalid url key"
      _  -> raiseStatus internalServerError500 "Unexpected result from DB"

  post "/api/:key" $ do
    -- FIXME: double check to prevent database DoS (sending huge json with
    -- invalid key)
    key <- param "key"
    Aeson.Success answers <- Aeson.fromJSON <$> jsonData
    res <- liftIO $ withResource pgPool $ \c ->
      PG.query c
        [qn| select post_customer_feedback(?, ?) |]
        (key :: Text, Aeson.encode (answers :: Feedback))
    json (join res :: [Int])

  get "/"
    $ file $ frontDir <> "index.html"

  get "/:file" $ do
    path <- param "file"
    -- NB. we replace dots to prevent escaping from `frontDir`
    let safePath = T.unpack $ T.replace ".." "-" path
    when (".svg" `T.isSuffixOf` path)
      $ setHeader "Content-Type" "image/svg+xml"
    file $ frontDir <> safePath
