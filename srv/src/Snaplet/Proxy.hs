module Snaplet.Proxy
    ( Proxy
    , proxyInit
    ) where


import           BasicPrelude
import           Prelude                       hiding (map, (++), (.))

import           Control.Exception.Lifted      (try)
import           Data.Aeson                    ((.=))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Configurator             as Cfg
import qualified Data.Configurator.Types       as Cfg
import qualified Data.Text                     as T
import           Network.HTTP.Simple           hiding (Proxy)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth             (AuthManager)
import           Snap.Snaplet.PostgresqlSimple (Postgres)

import qualified Carma.Model.Role              as Role
import           Carma.Utils.Snap              (withLens, writeJSON)
import           Snaplet.Auth.Class
import           Snaplet.Auth.PGUsers          (currentUserRoles)


data Proxy b = Proxy
    { auth :: SnapletLens b (AuthManager b)
    , db   :: SnapletLens b Postgres
    }

instance HasPostgresAuth b (Proxy b) where
    withAuth = withLens auth
    withAuthPg = withLens db


proxyInit
    :: SnapletLens b (AuthManager b)
    -> SnapletLens b Postgres
    -> SnapletInit b (Proxy b)
proxyInit auth db = makeSnaplet "proxy" "for reverse proxy requests" Nothing $ do
  cfg <- getSnapletUserConfig
  endPoints <- liftIO $ Cfg.lookupDefault [] cfg "end-points"
  handlers <- mapM (liftIO . getEndPoint cfg) $ map T.pack endPoints

  addRoutes $ zipWith (\dirName (prefix, cookie) ->
                           (BS.pack dirName, dirHandler prefix cookie)
                      ) endPoints handlers

  pure $ Proxy auth db

    where
      getEndPoint :: Cfg.Config -> Text -> IO (String, String)
      getEndPoint cfg name = do
            prefix <- liftIO $ Cfg.require cfg $ T.concat [name, ".prefix"]
            cookie <- liftIO $
                     fmap (fromMaybe "") $
                     Cfg.lookup cfg $ T.concat [name, ".cookie"]
            return (prefix, cookie)

      dirHandler :: String -> String -> Handler b (Proxy b) ()
      dirHandler prefix cookie = do
        roles <- currentUserRoles

        case roles of
          Nothing -> writeJSON $
                    Aeson.object [ "status"  .= T.pack "error"
                                 , "message" .= T.pack "Неавторизованный пользователь"
                                 ]

          Just rs ->
              if Role.core `elem` rs
              then do r <- getRequest

                      cookieVal <- if null cookie
                                  then return ""
                                  else getCookie (BS.pack cookie) >>=
                                       (return . maybe "" cookieValue)

                      resp <- try $
                             httpLBS $
                             setRequestHeader "Cookie" [cookieVal] $
                             setRequestHeader "Image-Prefix" [rqURI r] $
                             parseRequest_ $
                             "GET " ++ prefix ++ cleanupPath (rqPathInfo r)

                      let (contentType, content) =
                              case resp of
                                Left e ->
                                    ( "text/plain"
                                    , LBS.pack $ show (e :: HttpException)
                                    )
                                Right res' ->
                                    ( case getResponseHeader "content-type" res' of
                                        []    -> "application/json"
                                        (t:_) -> t
                                    , getResponseBody res'
                                    )

                      modifyResponse $ setContentType contentType
                      writeLBS content

              else writeJSON $
                   Aeson.object [ "status"  .= T.pack "error"
                                , "message" .= T.pack "Доступ запрещён"
                                ]

      cleanupPath :: ByteString -> String
      cleanupPath =
          T.unpack .
          T.intercalate "/" .
          filter (`notElem` ["", ".", ".."]) .
          T.split (=='/') .
          T.pack .
          BS.unpack
