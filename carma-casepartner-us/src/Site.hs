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
-- import           Control.Applicative
-- import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
--import           Data.List (intercalate)
-- import           Data.Map (Map)
-- import qualified Data.Map as Map
-- import           Data.Text (Text)
-- import qualified Data.Text as T
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
                 ( pgsInit
                 , query
                 , In (..)
                 )
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           AppHandlers.Util
-- import           AppHandlers.Users
import qualified Carma.Model.ServiceStatus as SS
import           Data.Model
--import           Data.Model.Utils.LegacyModel (identToRawFieldValue)


data LatestCases = Current
                 | Closing


casesLimit :: Int
casesLimit = 1000

apiLogin, apiLogout :: ByteString
apiLogin  = "/api/v1/login"
apiLogout = "/api/v1/logout"

apiGetLatestCurrentCases :: ByteString
apiGetLatestCurrentCases = "/api/v1/getLatestCases/current"

apiGetLatestClosingCases :: ByteString
apiGetLatestClosingCases = "/api/v1/getLatestCases/closing"
                            
                     
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
handleApiGetLatestCases :: LatestCases -> AppHandler ()
handleApiGetLatestCases caseType = do
  let statuses = case caseType of
                     Current -> [SS.ordered, SS.delayed, SS.inProgress]
                     Closing -> [SS.ok]

  rows <- query [sql|
    SELECT                   
      servicetbl.id::text,
      date_trunc('second', createTime::timestamp)::text as cTime,
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
    WHERE (servicetbl.status in ?) and (createTime is not null)
    ORDER BY cTime DESC
    LIMIT ?
  |] ( In $ map (\(Ident i) -> i)  statuses :: In [Int]
     , casesLimit :: Int
     )

  writeJSON $ mkMap ["id", "callDate", "typeOfService", "status"
                    , "accordTime", "breakdownPlace", "makeModel", "payType"
                    ] rows


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ (apiLogin,  with auth handleApiLogin)
         , (apiLogout, with auth handleApiLogout)
         , (apiGetLatestCurrentCases, handleApiGetLatestCases Current)
         , (apiGetLatestClosingCases, handleApiGetLatestCases Closing)
         , ("/login",  redirect "/")
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

