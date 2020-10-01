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
import           Snap.Snaplet.Auth hiding (session)
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple (pgsInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           AppHandlers.Users
import qualified AppHandlers.Services as Ss
import           AppHandlers.Service as S
import           AppHandlers.Dicts as D
import           Types


apiLogin, apiLogout :: ByteString
apiLogin  = "/api/v1/login"
apiLogout = "/api/v1/logout"


apiGetServices :: ByteString
apiGetServices = "/api/v1/services/all"

apiGetLatestCurrentServices :: ByteString
apiGetLatestCurrentServices = "/api/v1/services/current"

apiGetLatestClosingServices :: ByteString
apiGetLatestClosingServices = "/api/v1/services/closing"

apiGetService :: ByteString
apiGetService = "/api/v1/service/:serviceId"

apiGetServiceComments :: ByteString
apiGetServiceComments = "/api/v1/service/:serviceId/comments"

apiPostServiceComment :: ByteString
apiPostServiceComment = "/api/v1/case/:caseId/comment"

apiStatusInPlace :: ByteString
apiStatusInPlace = "/api/v1/service/:serviceId/inplace"

apiStatusServicePerformed :: ByteString
apiStatusServicePerformed = "/api/v1/service/:serviceId/performed"

apiPostPartnerDelay :: ByteString
apiPostPartnerDelay = "/api/v1/service/:serviceId/partnerdelay"

apiDictPartnerDelayReason :: ByteString
apiDictPartnerDelayReason = "/api/v1/dict/PartnerDelay_Reason"

apiMapTypeOfService :: ByteString
apiMapTypeOfService = "/api/v1/dict/TypeOfServiceSynonym"


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
         , (apiGetServices, method POST $ Ss.latestServices All)
         , (apiGetLatestCurrentServices, Ss.latestServices Current)
         , (apiGetLatestClosingServices, Ss.latestServices Closing)
         , (apiGetService, method GET S.handleApiGetService)
         , (apiGetServiceComments, method GET S.serviceComments)
         , (apiPostServiceComment, method POST S.postComment)

         , (apiStatusInPlace, method POST S.statusInPlace)
         , (apiStatusServicePerformed, method POST S.statusServicePerformed)

         , (apiPostPartnerDelay,       method POST S.postPartnerDelay)
         , (apiDictPartnerDelayReason, D.partnerDelayReason)
         , (apiMapTypeOfService,       D.typeOfService)

         , ("/login",           redirect "/")
         , ("/services",        redirect "/")
         , ("/show-service",    redirect "/")
         , ("/search",          redirect "/")

         , ("",        serveDirectoryWith fancyDirectoryConfig "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Case partner manager application." Nothing $ do
    s <- nestSnaplet "session" session $
        let lifetime = Just $ 365 * 24 * 60 * 60 -- one year in seconds
        in initCookieSessionManager "client_session_key.aes" "_session"
                                    Nothing lifetime

    ad <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth session ad
    addRoutes routes
    return $ App s ad a

