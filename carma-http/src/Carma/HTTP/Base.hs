{-|

Carma HTTP API monad and base types.

-}

module Carma.HTTP.Base
    (
      -- * CarmaIO monad
      CarmaOptions(..)
    , defaultCarmaOptions
    , CarmaIO
    , runCarma
      -- ** Monad operations
    , getPort
    , getHeaders
    , sendRequest
    , checkCode
      -- * URI building
    , modelURI
    , modelPidURI
    , methodURI
    )

where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Network.HTTP hiding (getHeaders)
import           Network.Stream             (Result)


-- | Options used to connect to a running CaRMa instance.
data CarmaOptions = CarmaOptions { carmaHost    :: String
                                 , carmaPort    :: Int
                                 , carmaHeaders :: [Header]
                                 }


defaultCarmaOptions :: CarmaOptions
defaultCarmaOptions = CarmaOptions "localhost" 8000 []


-- | A monad which keeps a single open connection for a sequence of
-- CaRMa requests.
type CarmaIO = ReaderT (CarmaOptions, HandleStream String) IO


runCarma :: CarmaOptions -> CarmaIO a -> IO a
runCarma opts action =
    bracket (openStream (carmaHost opts) (carmaPort opts))
            close
            (\s -> runReaderT action (opts, s))


getHost :: CarmaIO String
getHost = asks (carmaHost . fst)

getPort :: CarmaIO Int
getPort = asks (carmaPort . fst)


getHeaders :: CarmaIO [Header]
getHeaders = asks (carmaHeaders . fst)


sendRequest :: Request String -> CarmaIO (Result (Response String))
sendRequest req = do
  s <- asks snd
  liftIO $ sendHTTP s req


checkCode :: Monad m => (Int, Int, Int) -> m Bool
checkCode code =
  return $
   case code of
     (2, 0, 0) -> True
     (4, 0, 4) -> False
     _ -> error "Unexpected CaRMa response"


localhost :: String
localhost = "127.0.0.1"


-- | Model API endpoint.
modelURI :: Int
           -- ^ CaRMa port.
         -> String
         -- ^ Model name.
         -> String
modelURI cp model = concat ["http://", localhost, ":", show cp, "/_/", model, "/"]


-- | Model read/update/delete API endpoint.
modelPidURI :: Int -> String -> Int -> String
modelPidURI cp model pid = modelURI cp model ++ show pid


-- | Build URI used to call a method of local CaRMa.
methodURI :: String
          -- ^ Method name/call, like @repTowages/5003@ or @psaCases@,
          -- no trailing or leading slashes.
          -> CarmaIO String
methodURI meth = do
  h <- getHost
  p <- getPort
  return $ concat ["http://", h, ":", show p, "/", meth]
