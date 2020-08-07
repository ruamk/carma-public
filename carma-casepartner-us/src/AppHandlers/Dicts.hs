module AppHandlers.Dicts
    (partnerDelayReason
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
getDict dictName = query_ q >>= return . M.fromList
  where q = fromString $ "SELECT id, label FROM \"" ++ dictName ++ "\""


partnerDelayReason :: AppHandler ()
partnerDelayReason = checkAuthCasePartner $ do
  getDict "PartnerDelay_Reason" >>= writeJSON
