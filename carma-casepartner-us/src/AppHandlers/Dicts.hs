module AppHandlers.Dicts
    ( partnerDelayReason
    , typeOfService
    )
    where


import Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Map as M

import           Snap.Snaplet.PostgresqlSimple
                 ( query_
                 )

import Application
import AppHandlers.Users
import AppHandlers.Util


getDict :: String -> AppHandler (M.Map Int Text)
getDict dictName = M.fromList <$> query_ q
  where q = fromString $ "SELECT id, label FROM \"" ++ dictName ++ "\""


getMap :: String -> AppHandler (M.Map Text Text)
getMap mapName = M.fromList <$> query_ q
  where q = fromString $ "SELECT key, value FROM \"" ++ mapName ++ "\""


partnerDelayReason :: AppHandler ()
partnerDelayReason = checkAuthCasePartner $ do
  getDict "PartnerDelay_Reason" >>= writeJSON


typeOfService :: AppHandler ()
typeOfService = checkAuthCasePartner $ do
  getMap "TypeOfServiceSynonym" >>= writeJSON
