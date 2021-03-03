module AppHandlers.Dicts
    ( getDict
    , partnerDelayReason
    , typeOfService
    ) where


import qualified Data.Map                      as M
import           Data.String                   (fromString)
import           Data.Text                     (Text)

import           Snap.Snaplet.PostgresqlSimple (query_)

import           AppHandlers.Users
import           Application
import           Carma.Utils.Snap


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
