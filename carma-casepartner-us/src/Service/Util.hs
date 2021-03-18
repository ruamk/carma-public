module Service.Util
    ( carmaPostComment
    , carmaPostPartnerDelay
    , caseForService
    , getPhotoTypeParam
    , savePhoto
    , sendPhoto
    , serviceActionNotFound
    , serviceAlreadyPerformed
    , servicePerformed
    , setStatusClosed
    , setStatusInPlace
    , setStatusPartnerDelay
    , setStatusPerformed
    , withCookie
    ) where


import           Control.Exception                (bracket)
import           Control.Monad                    (void, when)
import           Control.Monad.IO.Class           (liftIO)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Configurator                (lookupDefault)
import qualified Data.Configurator                as Config
import           Data.List                        (find)
import qualified Data.Map                         as M
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time.Calendar               (toGregorian)
import           Data.Time.Clock                  (utctDay)
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap
import           Snap.Snaplet.PostgresqlSimple    (Only (..), query)
import           Snap.Util.FileUploads            (defaultUploadPolicy,
                                                   handleMultipart,
                                                   partContentType,
                                                   partFieldName, partFileName)
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath                  ((</>))
import           System.IO                        (hClose)
import qualified System.IO.Streams                as Streams
import           System.IO.Temp                   (openBinaryTempFile)

import           Application
import           Carma.Model
import           Carma.Model.Action               as Action
import qualified Carma.Model.ActionType           as ActionType
import           Carma.Model.Service              as Service
import qualified Carma.Model.ServiceStatus        as ServiceStatus
import           Carma.Utils.Snap                 (getDateTimeParam,
                                                   getIntParam,
                                                   getLatitudeParam,
                                                   getLongitudeParam)
import qualified CarmaApi
import qualified Data.Model.Patch                 as Patch
import           Types                            (DriverPhotoType (..))
import           Util                             (errorResponse, okResponse)


withCookie
    :: (MonadSnap (m b v), MonadSnaplet m)
    => (String -> m b v b1) -> m b v b1
withCookie f = do
  cfg <- getSnapletUserConfig

  cookieName :: String <- liftIO $ T.unpack . T.strip
                               <$> lookupDefault "_session" cfg "carma.cookie"

  c <- getCookie $ BS.pack cookieName

  case c of
    Nothing -> f ""
    Just c' -> f $ cookieName ++ "=" ++ BS.unpack (cookieValue c')


carmaUpdateFactServiceStartTime
    :: ( MonadSnaplet m
      , MonadSnap (m b v)
      )
    => IdentI Service
    -> m b v (Patch.Patch Service)
carmaUpdateFactServiceStartTime serviceId = withCookie $ \cookie -> do
  CarmaApi.updateFactServiceStartTime cookie serviceId


carmaServiceInProgress
    :: ( MonadSnaplet m
      , MonadSnap (m b v))
    => IdentI Action
    -> m b v (Patch.Patch Action)
carmaServiceInProgress actionId = withCookie $ \cookie -> do
  CarmaApi.serviceInProgress cookie actionId


carmaPostComment
    :: (MonadSnap (m b v), MonadSnaplet m)
    => Int
    -> String
    -> m b v Int
carmaPostComment caseId comment = withCookie $ \cookie -> do
  (Ident caseCommentId, _) <- CarmaApi.addComment cookie caseId comment
  return caseCommentId


carmaPostPartnerDelay
    :: (MonadSnap (m b v), MonadSnaplet m)
    => Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe Text
    -> m b v Int
carmaPostPartnerDelay caseId serviceId ownerId partnerId minutes reason comment =
  withCookie $ \cookie -> do
    (Ident partnerDelayId, _) <- CarmaApi.addPartnerDelay cookie
                                                         caseId serviceId
                                                         ownerId partnerId
                                                         minutes reason comment
    return partnerDelayId


-- Returns caseId for specified serviceId
caseForService :: Int -> AppHandler (Maybe Int)
caseForService serviceId =
  query "SELECT parentid FROM servicetbl where id = ?" (Only serviceId) >>=
  (return . \case
              [Only caseId] -> Just caseId
              _             -> Nothing)


setStatusClosed
    :: IdentI Service
    -> IdentI Action
    -> AppHandler (M.Map Text Text)
setStatusClosed serviceId checkActionId = do
  void $ withCookie $ \cookie ->
      CarmaApi.serviceClosed cookie serviceId checkActionId
  return serviceClosed


setStatusInPlace :: Int -> AppHandler (M.Map Text Text)
setStatusInPlace serviceId = do
  [(caseId, status)] <- query
                       "SELECT parentid, status FROM servicetbl WHERE id = ?" $
                       Only serviceId
  if status == ServiceStatus.ordered
  then do assigned <- isServiceAssignedToOperator serviceId
          if assigned -- назначена ли услуга на оператора
          then do
            _ <- carmaPostComment caseId "Партнёром произведён доезд до клиента"
            return servicePerformed
          else do
            -- происходит выполнения действия "Услуга в процессе оказания"
            v <- query [sql|
              SELECT id
              FROM actiontbl
              WHERE serviceId = ?
                AND type = ?
                AND result is null
            |] (serviceId, ActionType.checkStatus)

            case v of
              [Only (actionId :: IdentI Action)] -> do
                     _ <- carmaServiceInProgress actionId
                     -- update servicetbl.times в значение now
                     _ <- carmaUpdateFactServiceStartTime $ Ident serviceId
                     return servicePerformed
              _ -> return serviceActionNotFound

  else if status == ServiceStatus.ok
       then return serviceAlreadyPerformed
       else do
         _ <- carmaPostComment caseId "Партнёр приступил к оказанию услуги"
         return servicePerformed



setStatusPerformed :: Int -> String -> AppHandler (M.Map Text Text)
setStatusPerformed serviceId comment = do
  Just caseId <- caseForService serviceId
  _ <- carmaPostComment caseId comment
  return servicePerformed


setStatusPartnerDelay
    :: Int
    -> Int
    -> Int
    -> Int
    -> Maybe Text
    -> AppHandler Int
setStatusPartnerDelay serviceId uid minutes reason comment = do
  [Only (partnerId :: Int)] <- query
    "SELECT contractor_partnerid FROM servicetbl where id = ?"
    $ Only serviceId
  Just caseId <- caseForService serviceId
  when (minutes < 1) $ error "minutes should be > 0"

  carmaPostPartnerDelay caseId serviceId uid partnerId minutes reason comment


-- Услуга закрыта
serviceClosed :: M.Map Text Text
serviceClosed = serviceMessage "service_closed"

-- Услуга выполнена
servicePerformed :: M.Map Text Text
servicePerformed = serviceMessage "service_performed"

-- Услуга уже выполнена
serviceAlreadyPerformed :: M.Map Text Text
serviceAlreadyPerformed = serviceMessage "service_already_performed"

-- Действие не найдено
serviceActionNotFound :: M.Map Text Text
serviceActionNotFound = serviceMessage "action_not_found"

serviceMessage :: Text -> M.Map Text Text
serviceMessage message = M.fromList [ ( "status" :: Text
                                      , message
                                      )
                                    ]


isServiceAssignedToOperator :: Int -> AppHandler Bool
isServiceAssignedToOperator serviceId = do
  [Only (count :: Int)] <- query [sql|
    SELECT count(*)
    FROM actiontbl
    WHERE serviceid = ?
      AND result IS NULL
      AND assignedto IS NOT NULL
      AND type = ?
  |] (serviceId, ActionType.checkStatus)
  return $ count > 0


getPhotoTypeParam :: ByteString -> Handler a b (Either Text DriverPhotoType)
getPhotoTypeParam name =
  getParam name >>= \v ->
      return $ case v of
                 Just "after"     -> Right DriverPhotoAfter
                 Just "before"    -> Right DriverPhotoBefore
                 Just "difficult" -> Right DriverPhotoDifficult
                 Just "order"     -> Right DriverPhotoOrder
                 Just iv          -> Left $ T.concat [ "invalid value '"
                                                    , T.pack $ BS.unpack iv
                                                    , "' for parameter "
                                                    , T.pack $ BS.unpack name
                                                    ]
                 Nothing          -> Left $ T.concat [ T.pack $ BS.unpack name
                                                    , " not specified"
                                                    ]


savePhoto :: Maybe Int -> AppHandler ()
savePhoto driverId  = do
  parts <- handleMultipart defaultUploadPolicy partHandler

  let image = case find (\(name, _, _, _) -> "image" == name) parts of
                Nothing -> Left $ T.pack "image is not specified"

                Just (_, _, "image/jpeg", rawData) ->
                    if BS.null rawData
                    then Left $ T.pack "image is empty"
                    else Right rawData

                Just _ -> Left $ T.pack "image Content-Type is not image/jpeg"

  serviceId   <- getIntParam "serviceId"
  latitude    <- getLatitudeParam "latitude"
  longitude   <- getLongitudeParam "longitude"
  created     <- getDateTimeParam "created"
  photoType   <- getPhotoTypeParam "type"

  case (image, serviceId, latitude, longitude, created, photoType) of
    (Left err, _, _, _, _, _) -> errorResponse err

    (_, Nothing, _, _, _, _)  ->
        errorResponse "serviceId is not specified or contains invalid value"

    (_, _, Left err, _, _, _) ->  errorResponse err

    (_, _, _, Left err, _, _) ->  errorResponse err

    (_, _, _, _, Nothing, _) ->
        errorResponse "created is not specified or contains invalid value"

    (_, _, _, _, _, Left err) ->  errorResponse err

    ( Right rawPhoto, Just serviceId'
     , Right lat, Right lon, Just created', Right photoType') -> do
          filename <- savePhotoFile rawPhoto serviceId' photoType' created'
          photoId  <- savePhotoInfo serviceId' lat lon created'
                                   photoType' filename
          okResponse $ T.pack $ show photoId

  where
    partHandler pInfo stream = do
      let fieldname   = BS.unpack $ partFieldName pInfo
          filename    = partFileName pInfo
          contentType = BS.unpack $ partContentType pInfo
      body <- BS.concat <$> Streams.toList stream
      return (fieldname, filename, contentType, body)


    savePhotoInfo serviceId lat lon created photoType filename = do
      [Only photoId] :: [Only Int] <- query [sql|
        INSERT INTO driversPhotos
                    (driverId, serviceId, coord, created, photoType, filename)
             VALUES (?, ?, ST_SetSRID(ST_MakePoint(?, ?), 4326), ?, ?, ?)
          RETURNING id
      |] (driverId, serviceId, lon, lat, created, photoType, filename)
      return photoId


    savePhotoFile rawPhoto serviceId photoType created = do
      cfg <- getSnapletUserConfig
      dirPrefix <- liftIO $ Config.require cfg "photo.directory"

      let (year, m, _) = toGregorian $ utctDay created
          month = (if m < 10 then "0" else "") ++ show m
          dirname = dirPrefix </> show photoType </> show year </> month
          filenameTemplate = show serviceId ++ "_.jpg"

      liftIO $ createDirectoryIfMissing True dirname
      f <- liftIO $ bracket (openBinaryTempFile dirname filenameTemplate)
                           (\(_, h) -> hClose h)
                           (\(filePath, h) ->
                                BS.hPut h rawPhoto >>
                                  return (drop (length dirPrefix) filePath)
                           )
      return f


sendPhoto :: String -> AppHandler ()
sendPhoto filename = do
  cfg <- getSnapletUserConfig
  dirPrefix <- liftIO $ Config.require cfg "photo.directory"
  modifyResponse $ setContentType "image/jpeg"
  sendFile $ dirPrefix </> filename
