------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
    ( app
    ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class                      (liftIO)
import           Data.ByteString                             (ByteString)
import           Data.Configurator                           as Config
import           Data.Maybe                                  (fromMaybe, isJust)
import qualified Data.Text.Encoding                          as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth                           hiding (session)
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple               (pgsInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           AppHandlers.Dicts                           as D
import           AppHandlers.MobileAPI                       as MobileAPI
import           AppHandlers.Service                         as S
import qualified AppHandlers.Services                        as Ss
import           AppHandlers.Settings.Drivers                as Drivers
import           AppHandlers.Users
import           Application
import qualified PgNotify
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

apiGetServiceLocation :: ByteString
apiGetServiceLocation = "/api/v1/service/:serviceId/location"

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

-- API for ANDROID application
apiDriverLogin :: ByteString
apiDriverLogin = "/api/v1/driver/login"

apiDriverOrder :: ByteString
apiDriverOrder = "/api/v1/driver/order"

apiDriverLocation :: ByteString
apiDriverLocation = "/api/v1/driver/location"

apiDriverDelayReason :: ByteString
apiDriverDelayReason = "/api/v1/driver/delayreason"

apiDriverStatus :: ByteString
apiDriverStatus = "/api/v1/driver/status"

apiDriverSettings :: ByteString
apiDriverSettings = "/api/v1/driver/settings"

apiGetDrivers :: ByteString
apiGetDrivers = "/api/v1/settings/drivers" -- GET

apiCreateDriver :: ByteString
apiCreateDriver = "/api/v1/settings/driver" -- POST

apiDriver :: ByteString
apiDriver = "/api/v1/settings/driver/:driverId" -- GET for location,
                                                -- PUT for update,
                                                -- DELETE for delete,
                                                -- POST for send SMS to driver.


apiAssignServiceToDriver :: ByteString
apiAssignServiceToDriver = "/api/v1/assignservice/:serviceId/:driverId"

apiCancelServiceToDriver :: ByteString
apiCancelServiceToDriver = "/api/v1/cancelservice/:serviceId/:driverId"


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
         , (apiGetServiceLocation, method GET S.serviceLocation)

         , (apiStatusInPlace, method POST S.statusInPlace)
         , (apiStatusServicePerformed, method POST S.statusServicePerformed)

         , (apiPostPartnerDelay,       method POST S.postPartnerDelay)
         , (apiDictPartnerDelayReason, D.partnerDelayReason)
         , (apiMapTypeOfService,       D.typeOfService)

         , (apiDriverLogin,            method POST MobileAPI.login)
         , (apiDriverStatus,           method POST MobileAPI.status)
         , (apiDriverOrder,            method GET  MobileAPI.order)
         , (apiDriverLocation,         method POST MobileAPI.location)
         , (apiDriverDelayReason,      method GET  MobileAPI.delayReason)
         , (apiDriverSettings,         method GET  MobileAPI.settings)

         , (apiGetDrivers,             method GET    Drivers.getDrivers)
         , (apiCreateDriver,           method POST   Drivers.createDriver)
         , (apiDriver,                 method GET    Drivers.showDriverLocation)
         , (apiDriver,                 method PUT    Drivers.updateDriver)
         , (apiDriver,                 method DELETE Drivers.deleteDriver)
         , (apiDriver,                 method POST   Drivers.sendSMS)
         , (apiAssignServiceToDriver,  method GET    Drivers.assignService)
         , (apiCancelServiceToDriver,  method GET    Drivers.cancelService)

         , ("/login",           redirect "/")
         , ("/services",        redirect "/")
         , ("/show-service",    redirect "/")
         , ("/search",          redirect "/")
         , ("/settings",        redirect "/")
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

    config <- getSnapletUserConfig

    wwwDir <- liftIO $ Config.require config "www-dir"
    addRoutes routes
    addRoutes [("",        serveDirectoryWith fancyDirectoryConfig wwwDir)]

    liftIO $ PgNotify.startLoop config

    return $ App s ad a
