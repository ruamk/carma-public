------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
                 ( pgsInit
                 )
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           AppHandlers.Users
import qualified AppHandlers.Services as Ss
import           AppHandlers.CaseDescription as CD
import           Types


apiLogin, apiLogout :: ByteString
apiLogin  = "/api/v1/login"
apiLogout = "/api/v1/logout"

apiGetLatestCurrentServices :: ByteString
apiGetLatestCurrentServices = "/api/v1/services/current"

apiGetLatestClosingServices :: ByteString
apiGetLatestClosingServices = "/api/v1/services/closing"

apiGetService :: ByteString
apiGetService = "/api/v1/getService/:serviceId"

apiGetCaseComments :: ByteString
apiGetCaseComments = "/api/v1/getServiceComments/:caseId"


-- | Handle login API
handleApiLogin :: AppHandler ()
handleApiLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- isJust <$> getParam "remember"
  res <- with auth $ loginByUsername (T.decodeUtf8 l) (ClearText p) r
  case res of
    Left _  -> redirect' apiLogin 401 -- login again
    Right _ -> checkAuthCasePartner $ redirect "/"


-- | Handle logout API
handleApiLogout :: AppHandler ()
handleApiLogout = ifTop $ do
  with auth logout
  redirect apiLogin



------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ (apiLogin,  method POST handleApiLogin)
         , (apiLogout, method POST handleApiLogout)
         , (apiGetLatestCurrentServices, Ss.latestServices Current)
         , (apiGetLatestClosingServices, Ss.latestServices Closing)
         , (apiGetService, method GET CD.handleApiGetService)
         , (apiGetCaseComments, CD.handleApiGetCaseComments)
         , ("",        serveDirectoryWith fancyDirectoryConfig "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Case partner manager application." Nothing $ do
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    ad <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess ad
    addRoutes routes
    return $ App s ad a

