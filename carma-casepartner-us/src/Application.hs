------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Class
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session
import Snaplet.Auth.Class

------------------------------------------------------------------------------
data App = App
    { _sess  :: Snaplet SessionManager
    , _db    :: Snaplet Postgres
    , _auth  :: Snaplet (AuthManager App)
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App


instance HasPostgresAuth App App where
    withAuth = with auth
    withAuthPg = with db

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

