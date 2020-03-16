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
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Encoding as T
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
import           AppHandlers.Users
import           AppHandlers.Util
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.Role as Role
import           Data.Model


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


checkAuthCasePartner :: AppHandler () -> AppHandler ()
checkAuthCasePartner m = do
  chkUserActiveness
  chkAuthRoles (hasAnyOfRoles [Role.casePartner]) m

                     
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


-- | Handle get latest cases
handleApiGetLatestCases :: LatestCases -> AppHandler ()
handleApiGetLatestCases caseType = checkAuthCasePartner $ do
  user <- fromMaybe (error "No current user") <$> with auth currentUser
  let UserId uid = fromMaybe (error "no uid") $ userId user  
  let statuses = case caseType of
                     Current -> [SS.ordered, SS.delayed, SS.inProgress]
                     Closing -> [SS.ok]

  rows <- query [sql|
    SELECT                   
      servicetbl.parentid::text,
      date_trunc('second', createTime::timestamp)::text as cTime,
      st.label,
      ss.label,
      date_trunc('second', times_expectedservicestart::timestamp)::text,
      ct.caseaddress_address,
      cma.label || ' / ' || cmo.label,
      pt.label 
    FROM servicetbl
    LEFT OUTER JOIN "ServiceType" st ON st.id = type
    LEFT OUTER JOIN "ServiceStatus" ss ON ss.id = status
    LEFT OUTER JOIN "PaymentType" pt ON pt.id = paytype
    LEFT OUTER JOIN casetbl ct ON ct.id = parentid
    LEFT OUTER JOIN "CarMake" cma ON cma.id = ct.car_make
    LEFT OUTER JOIN "CarModel" cmo ON cmo.id = ct.car_model
    LEFT OUTER JOIN "CasePartner" cp ON cp.partner = servicetbl.contractor_partnerid
    WHERE (servicetbl.status in ?)
      and (createTime is not null)
      and cp.uid = ?
    ORDER BY cTime DESC
    LIMIT ?
  |] ( In $ map (\(Ident i) -> i)  statuses :: In [Int]
     , uid
     , casesLimit :: Int
     )

  writeJSON $ mkMap ["id", "callDate", "typeOfService", "status"
                    , "accordTime", "breakdownPlace", "makeModel", "payType"
                    ] rows


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ (apiLogin,  method POST $ handleApiLogin)
         , (apiLogout, method POST $ handleApiLogout)
         , (apiGetLatestCurrentCases, handleApiGetLatestCases Current)
         , (apiGetLatestClosingCases, handleApiGetLatestCases Closing)
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

