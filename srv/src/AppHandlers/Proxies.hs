
{-|

Rukovoditel Koll Centra screen.

-}

module AppHandlers.Proxies
    (
      dadataAbbrevProxy
    )

where

import           Control.Monad.IO.Class
import           Control.Monad.State.Class

import           Data.Aeson

import           Snap

import qualified Network.Wreq                       as NW

import           Application
import           Util

dadataAbbrevProxy :: AppHandler ()
dadataAbbrevProxy = logExceptions ("handler/abbrevproxy") $ do
  _token <- gets dadataToken
  Just js <- fmap decode $ readRequestBody 10000 :: AppHandler (Maybe Value)
  r <- liftIO $ NW.postWith (NW.defaults) "https://suggestions.dadata.ru/suggestions/api/4_1/rs/suggest/address" js
  return ()

