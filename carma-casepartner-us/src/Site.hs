{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Syntax ((##))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple (pgsInit, query_, withPG, Postgres)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           AppHandlers.Util
-- import           AppHandlers.Users


apiLogin, apiLogout :: ByteString
apiLogin  = "/api/v1/login"
apiLogout = "/api/v1/logout"

apiGetLastestCases :: ByteString
apiGetLastestCases = "/api/v1/getLatestCases"

                     
-- | Handle login API
handleApiLogin :: Handler App (AuthManager App) ()
handleApiLogin = do
  loginUser "login" "password" Nothing
              (\_ -> redirect' apiLogin 401) -- login again
              (finishWith emptyResponse)


-- | Handle logout API
handleApiLogout :: Handler App (AuthManager App) ()
handleApiLogout = logout >> redirect apiLogin


-- | Handle get latest cases
handleApiGetLatestCases :: AppHandler ()
handleApiGetLatestCases = do
  rows <- query_ $ [sql|
    SELECT                   
      servicetbl.id::text,
      date_trunc('second', createTime::timestamp)::text,
      st.label,
      ss.label,
      date_trunc('second', times_expectedservicestart::timestamp)::text,
      contractor_address,
      cma.label || ' / ' || cmo.label,
      pt.label 
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" st ON st.id = type
    LEFT OUTER JOIN "ServiceStatus" ss ON ss.id = status
    LEFT OUTER JOIN "PaymentType" pt ON pt.id = paytype
    LEFT OUTER JOIN casetbl ct ON ct.id = parentid
    LEFT OUTER JOIN "CarMake" cma ON cma.id = ct.car_make
    LEFT OUTER JOIN "CarModel" cmo ON cmo.id = ct.car_model
    LIMIT 120
  |]
  writeJSON $ mkMap ["id", "callDate", "typeOfService", "status"
                    , "accordTime", "breakdownPlace", "makeModel", "payType"
                    ] rows


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ (apiLogin,  with auth handleApiLogin)
         , (apiLogout, with auth handleApiLogout)
         , (apiGetLastestCases, handleApiGetLatestCases)
         , ("/login",  redirect "/")
         , ("",        serveDirectoryWith fancyDirectoryConfig "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    ad <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess ad
    addRoutes routes
    return $ App s ad a

