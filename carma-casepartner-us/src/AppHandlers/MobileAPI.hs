{- This module provide API for mobile application. -}

module AppHandlers.MobileAPI
    ( androidVersionAndURL
    , delayReason
    , getPhoto
    , getPhotos
    , location
    , login
    , order
    , savePhoto
    , settings
    , status
    ) where


import           Control.Exception                    (SomeException, bracket,
                                                       handle)
import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), Value (..),
                                                       eitherDecode,
                                                       genericToJSON, object,
                                                       withObject, (.:))
import           Data.Aeson.Types                     (constructorTagModifier,
                                                       defaultOptions,
                                                       fieldLabelModifier)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.Char                            (toLower)
import qualified Data.Configurator                    as Config
import qualified Data.Configurator.Types              as Config
import           Data.List                            (find)
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Data.Time.Calendar                   (toGregorian)
import           Data.Time.LocalTime                  (ZonedTime, localDay,
                                                       zonedTimeToLocalTime)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       ResultError (..),
                                                       returnError, typename)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           GHC.Generics
import           GHC.Word                             (Word64)
import           Snap
import           Snap.Snaplet.PostgresqlSimple        (Only (..), execute,
                                                       query)
import           Snap.Util.FileUploads                (defaultUploadPolicy,
                                                       handleMultipart,
                                                       partContentType,
                                                       partFieldName,
                                                       partFileName)
import           System.Directory                     (createDirectoryIfMissing)
import           System.FilePath                      ((</>))
import           System.IO                            (hClose)
import qualified System.IO.Streams                    as Streams
import           System.IO.Temp                       (openBinaryTempFile)
import           Text.Read                            (readMaybe)
import           Web.ClientSession

import           AppHandlers.Dicts
import           Application
import           Carma.Model
import qualified Carma.Model.Usermeta                 as Usermeta
import           Carma.Utils.Snap
import           Service.Util


data LoginForm = LoginForm
    { username :: Text
    , password :: Text
    } deriving (FromJSON, Generic, Show)


data Location = Location
    { latitude  :: Double
    , longitude :: Double
    } deriving (FromJSON, ToJSON, Generic, Show)


data ServiceStatus = ServiceStatusInPlace
                   | ServiceStatusPerformed
                   | ServiceStatusDelay
                     { _minutes  :: Int
                     , _reason   :: Int
                     , _comments :: Maybe Text
                     }
    deriving (Generic, Show)

instance ToJSON ServiceStatus where
    toJSON ServiceStatusInPlace   = object [ ("status", "inplace")]
    toJSON ServiceStatusPerformed = object [ ("status", "performed")]
    toJSON (ServiceStatusDelay m r c) =
        object [ ("status",   "delay")
               , ("minutes",  Number $ fromIntegral m)
               , ("reason",   Number $ fromIntegral r)
               , ("comments", maybe Null String c
                 )
               ]

instance FromJSON ServiceStatus where
    parseJSON = withObject "service status" $ \o -> do
                  s <- o .: "status"
                  case s of
                    "inplace" -> return ServiceStatusInPlace
                    "performed" -> return ServiceStatusPerformed
                    "delay" -> ServiceStatusDelay
                                                   <$> o .: "minutes"
                                                   <*> o .: "reason"
                                                   <*> o .: "comments"
                    _ -> fail ("unknown status: " ++ s)


data Service = Service
    { serviceId            :: Int
    , serviceType          :: Maybe Text
    , caseId               :: Int
    , client               :: Maybe Text
    , clientPhone          :: Maybe Text
    , firstAddress         :: Maybe Text
    , lastAddress          :: Maybe Text
    , firstLocation        :: Maybe Location
    , _lastLocation        :: Maybe Location
    , expectedServiceStart :: Maybe ZonedTime
    , factServiceStart     :: Maybe ZonedTime
    , factServiceEnd       :: Maybe ZonedTime
    , makeModel            :: Maybe Text
    , plateNum             :: Maybe Text
    , vin                  :: Maybe Text
    , payType              :: Maybe Text
    , serviceStatus        :: Maybe Text -- ServiceStatus
    , _driverStatus        :: Maybe Text
    } deriving (Generic, Show)

instance ToJSON Service where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

emptyService :: Service
emptyService = Service 0 Nothing 0
                  Nothing Nothing Nothing Nothing
                  Nothing Nothing
                  Nothing Nothing Nothing
                  Nothing Nothing Nothing
                  Nothing Nothing Nothing


data DriverPhotoType = DriverPhotoBefore
                     | DriverPhotoAfter
                     | DriverPhotoDifficult
                     | DriverPhotoOrder
                       deriving (Generic, Read)

instance Show DriverPhotoType where
    show DriverPhotoBefore    = "Before"
    show DriverPhotoAfter     = "After"
    show DriverPhotoDifficult = "Difficult"
    show DriverPhotoOrder     = "Order"

instance ToField DriverPhotoType where
    toField = toField . show

instance FromField DriverPhotoType where
    fromField f mdata = do
      typeName <- typename f
      if typeName /= "DriverPhotoType"
      then returnError Incompatible f ""
      else case BS.unpack `fmap` mdata of
             Nothing -> returnError UnexpectedNull f ""
             Just v  -> case readMaybe ("DriverPhoto" ++ v) of
                         Nothing -> returnError ConversionFailed f
                                               "mismatched enums"
                         Just v' -> return v'


instance ToJSON DriverPhotoType where
    toJSON = genericToJSON defaultOptions
             { constructorTagModifier = map toLower . drop 11 }


data PhotoInfo = PhotoInfo
    { _driverId  :: Int
    , _serviceId :: Int
    , _latitude  :: Maybe Double
    , _longitude :: Maybe Double
    , _created   :: Maybe ZonedTime
    , _type      :: Maybe DriverPhotoType
    , image      :: Maybe Text
    } deriving (Generic, Show)

instance ToJSON PhotoInfo where
    toJSON = genericToJSON defaultOptions
             { fieldLabelModifier = dropWhile (== '_')}

instance FromRow PhotoInfo where
    fromRow = PhotoInfo <$> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field

requestSize :: Word64
requestSize = 4096

sessionCookieName :: ByteString
sessionCookieName = "_session"


response :: Text -> Text -> Value
response s m = object [ ("status",  String s)
                      , ("message", String m)
                      ]


okResponse :: Text -> AppHandler ()
okResponse message = writeJSON $ response "ok" message


errorResponse :: Text -> AppHandler ()
errorResponse message = writeJSON $ response "error" message


-- todo: использовать isActive в проверке
isValidDriver :: AppHandler (Maybe Int)
isValidDriver = do
  cookie <- getCookie sessionCookieName
  case cookie of
    Nothing -> return Nothing
    Just c  -> do isDriver <- checkCookie $ cookieValue c
                  case isDriver of
                    Just driverId -> return $ Just driverId
                    Nothing       -> return Nothing

  where checkCookie :: ByteString -> AppHandler (Maybe Int)
        checkCookie cookie = do
          res :: [Only Int] <- query [sql|
            SELECT id
              FROM "CasePartnerDrivers"
             WHERE cookie = ?
               AND isActive
          |] (Only cookie)

          return $ case res of
            [Only driverId] -> Just driverId
            _               -> Nothing


checkDriver :: (Int -> AppHandler ()) -> AppHandler ()
checkDriver m = do
  r <- isValidDriver
  case r of
    Just driverId -> m driverId
    Nothing -> do modifyResponse $ setResponseCode 401
                  writeBS ""


getCurrentService :: Int -> AppHandler (Maybe Int)
getCurrentService driverId = do
  hasService :: [Only (Maybe Int)] <- query [sql|
    SELECT currentServiceId
      FROM "CasePartnerDrivers"
     WHERE id = ?
  |] $ Only driverId

  case hasService of
    [Only (Just serviceId)] -> return $ Just serviceId
    _                       -> return Nothing


-- | Вход в систему из мобильного приложения
login :: AppHandler ()
login = do
  body <- readRequestBody requestSize
  let isLogin = eitherDecode body :: Either String LoginForm

  case isLogin of
    Left e -> errorResponse $ T.pack e
    Right form ->
      if T.null $ username form
      then errorResponse "Имя пользователя не указано."
      else if T.null $ password form
           then errorResponse "Пароль не указан."
           else do
             valid <- checkDriverLogin (username form) (password form)
             case valid of
               Just cookie -> do modifyResponse $ addResponseCookie cookie
                                 okResponse ""
               Nothing     -> errorResponse "Имя пользователя или пароль указаны неверно."

  where
    checkDriverLogin :: Text -> Text -> AppHandler (Maybe Cookie)
    checkDriverLogin username password = do
      -- todo: encrypt password with sha256
      res :: [Only Int] <- query [sql|
        SELECT id
          FROM "CasePartnerDrivers"
         WHERE phone = ? and password = ?
      |] ( username
         , password
         )

      case res of
        [Only driverId] -> do
          cfg <- getSnapletUserConfig
          keyFileName <- liftIO $ Config.require cfg "site-key"
          key <- liftIO $ getKey keyFileName
          cookieValue <- liftIO $ encryptIO key $ BS.pack $ T.unpack password
          saveCookie driverId cookieValue
          return $ Just
                 $ Cookie sessionCookieName cookieValue
                          Nothing Nothing Nothing False False

        _ -> return Nothing

    saveCookie :: Int -> ByteString -> AppHandler ()
    saveCookie driverId cookie = void $
      execute [sql|
        UPDATE "CasePartnerDrivers"
           SET cookie = ?
         WHERE id = ?
      |] ( cookie
         , driverId
         )


-- | Статус заказа
order :: AppHandler ()
order = checkDriver $ \driverId -> do
  hasService <- getService driverId

  case hasService of
    Just service -> writeJSON [service]
    Nothing      -> writeJSON ([] :: [Service])

  where
    getService :: Int -> AppHandler (Maybe Service)
    getService driverId = do
      hasService <- getCurrentService driverId

      case hasService of
        Just serviceId -> do
          [( client, clientPhone, firstAddress, firstLonLat, makeModel
           , plateNum, vin, caseId)] <- query [sql|
            SELECT contact_name
                 , contact_phone1
                 , caseAddress_address
                 , coalesce(caseAddress_coords, ''::text)
                 , make.label || ' / ' ||
                   regexp_replace(model.label, '^([^/]*)/.*','\1')
                 , car_plateNum
                 , car_vin
                 , casetbl.id
              FROM casetbl
              LEFT OUTER JOIN "CarMake"  make  ON make.id  = car_make
              LEFT OUTER JOIN "CarModel" model ON model.id = car_model
              LEFT OUTER JOIN servicetbl ON servicetbl.parentid = casetbl.id
             WHERE servicetbl.id = ?
          |] $ Only serviceId

          [( expectedServiceStart, factServiceStart, factServiceEnd
           , serviceType, serviceStatus, payType )] <- query [sql|
            SELECT times_expectedServiceStart
                 , times_factServiceStart
                 , times_factServiceEnd
                 , st.label
                 , ss.label
                 , pt.label
              FROM servicetbl
              LEFT OUTER JOIN "ServiceType" st   ON servicetbl.type = st.id
              LEFT OUTER JOIN "ServiceStatus" ss ON servicetbl.status = ss.id
              LEFT OUTER JOIN "PaymentType" pt   ON servicetbl.paytype = pt.id
             WHERE servicetbl.id = ?
          |] $ Only serviceId

          let firstLocation = if T.null firstLonLat
                              then Nothing
                              else let [lon, lat] = map (read . T.unpack) $
                                                    T.split (==',') firstLonLat
                                   in Just $ Location lat lon

          lastAddress <- query [sql|
                          WITH service AS (
                            SELECT id, towaddress_address FROM towagetbl
                             UNION ALL
                            SELECT id, towaddress_address FROM "BikeTowage"
                          )
                          SELECT coalesce(towaddress_address, '')
                            FROM service
                           WHERE id = ?
                           LIMIT 1
                        |] (Only serviceId)
                        >>= \r -> return $ case r of
                                            [Only a] -> Just a
                                            _        -> Nothing


          return $ Just
                 $ emptyService { serviceId = serviceId
                                , serviceType = serviceType
                                , caseId = caseId
                                , client = client
                                , clientPhone = clientPhone
                                , firstAddress = firstAddress
                                , firstLocation = firstLocation
                                , lastAddress = lastAddress
                                , expectedServiceStart = expectedServiceStart
                                , factServiceStart = factServiceStart
                                , factServiceEnd = factServiceEnd
                                , makeModel = makeModel
                                , plateNum = plateNum
                                , vin = vin
                                , payType = payType
                                , serviceStatus = serviceStatus
                                }

        _ -> return Nothing


invalidLatitude, invalidLongitude :: Text
invalidLatitude = "Значение параметра latitude должно быть в диапазоне -90 .. 90."
invalidLongitude = "Значение параметра longitude должно быть в диапазоне -180 .. 180."


-- | Обновление местоположения
location :: AppHandler ()
location = checkDriver $ \driverId -> do
  body <- readRequestBody requestSize
  let isLocation = eitherDecode body :: Either String Location
  case isLocation of
    Left e -> errorResponse $ T.pack e
    Right form -> do
      let (lat, lon) = (latitude form, longitude form)
      if lat < -90.0 || lat > 90.0
      then errorResponse invalidLatitude
      else if lon < -180.0 || lon > 180.0
           then errorResponse invalidLongitude
           else do updateLocation driverId lat lon
                   -- тут добавить комментарий, Водитель приступил к оказанию услуги
                   -- вы можете посмотреть его местопопложение на карте
                   okResponse ""

  -- todo: проверять isActive
  where updateLocation :: Int -> Double -> Double -> AppHandler ()
        updateLocation driverId lat lon = do
          r :: [Only (Maybe Int)] <- query [sql|
            SELECT currentServiceId
              FROM "CasePartnerDrivers"
             WHERE id = ?
          |] $ Only driverId

          case r of
            [Only (Just serviceId)] -> do
              -- todo: check UPSERT here
              [Only firstLocation] :: [Only Bool] <- query [sql|
                SELECT locations IS NULL
                  FROM driverServiceQueue
                 WHERE serviceId = ?
              |] $ Only serviceId

              if firstLocation
              then do
                void $ execute [sql|
                  UPDATE driverServiceQueue
                     SET locations = '[(?, ?)]' :: path
                       , timeline = ARRAY [now() :: timestamp with time zone]
                   WHERE serviceId = ?
                |] ( lat
                   , lon
                   , serviceId
                   )
                void $ execute "NOTIFY driver_service_response, ?"
                     $ Only $ show serviceId

              else do
                void $ execute [sql|
                  UPDATE driverServiceQueue
                     SET locations = locations + ('[(?, ?)]' :: path)
                       , timeline = ARRAY_APPEND(timeline, now() :: timestamp with time zone)
                   WHERE serviceId = ?
                  |] ( lat
                     , lon
                     , serviceId
                     )

            _ -> return ()

-- | Смена статуса: "на месте", "оказана" или "задержка"
status :: AppHandler ()
status = checkDriver $ \driverId -> do
  body <- readRequestBody requestSize
  let isServiceStatus = eitherDecode body :: Either String ServiceStatus

  case isServiceStatus of
    Left e -> errorResponse $ T.pack $ "Неправильные параметры запроса: " ++ e

    Right serviceStatus -> do
      hasService <- getCurrentService driverId

      case hasService of
        Nothing -> errorResponse "Услуга не найдена!"
        Just serviceId -> do
            case serviceStatus of
              ServiceStatusInPlace -> do
                setStatusInPlace serviceId
                okResponse ""

              ServiceStatusPerformed -> do
                setStatusPerformed serviceId $
                      "Партнёр выполнил услугу " ++ show serviceId
                closeService serviceId
                okResponse ""

              ServiceStatusDelay minutes reason comments ->
                checkReason reason $ do
                  let Ident uid = Usermeta.admin
                  i <- setStatusPartnerDelay serviceId uid
                                            minutes reason comments
                  okResponse $ T.pack $ show i

      where
        checkReason r m = do
          d <- getDict "PartnerDelay_Reason"
          case M.lookup r d of
            Nothing -> errorResponse $ T.pack
                                    $ "Причина задержки указана неверно: " ++
                                      show r
            Just _  -> m

        closeService serviceId = do
          void $ execute [sql|
            UPDATE driverServiceQueue
               SET closed = now()
             WHERE serviceId = ?
          |] $ Only serviceId

          void $ execute [sql|
            UPDATE "CasePartnerDrivers"
               SET currentServiceId = NULL
             WHERE id = ?
          |] $ Only driverId


-- | Возвращает настройки для мобильного приложения
settings :: AppHandler ()
settings = checkDriver $ \_ -> do
  cfg <- getSnapletUserConfig
  updateInterval :: Int <- liftIO $ Config.lookupDefault 60 cfg
                              "android.location-update-interval"

  (version, url) <- androidVersionAndURL cfg

  writeJSON $ object [ ("version", String version)
                     , ("updateUrl", String url)
                     , ("locationUpdateInterval", Number $ fromIntegral updateInterval)
                     ]


androidVersionAndURL :: MonadSnap m => Config.Config -> m (Text, Text)
androidVersionAndURL cfg = do
  versionFile <- liftIO $ Config.require cfg "android.version-file"
  urlPrefix   <- liftIO $ Config.require cfg "android.update-url.prefix"
  urlSuffix   <- liftIO $ Config.require cfg "android.update-url.suffix"

  r <- liftIO $
      handle (\e -> return (Left (show (e :: SomeException))))
             (do version <- T.strip <$> T.readFile versionFile
                 return $ Right version
             )
  case r of
       Left e -> do
         logError $ BS.pack $ "Unable to read version of mobile application: " ++ e
         let v = "0000000000"
         return (v, T.concat [urlPrefix, v, urlSuffix])

       Right v ->
         return (v, T.concat [urlPrefix, v, urlSuffix])


-- | Возвращает справочник причин задержки
delayReason :: AppHandler ()
delayReason = checkDriver $ \_ -> getDict "PartnerDelay_Reason" >>= writeJSON


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


savePhoto :: AppHandler ()
savePhoto = checkDriver $ \driverId -> do
  parts <- handleMultipart defaultUploadPolicy partHandler

  let image = case find (\(name, _, _, _) -> "image" == name) parts of
                Nothing -> Left $ T.pack "image is not specified"

                Just (_, _, "image/jpeg", rawData) ->
                    if BS.null rawData
                    then Left $ T.pack "image is empty"
                    else Right rawData

                Just _ -> Left $ T.pack "image Content-Type is not image/jpeg"

  serviceId <- getIntParam "serviceId"
  latitude  <- getLatitudeParam "latitude"
  longitude <- getLongitudeParam "longitude"
  created   <- getDateTimeParam "created"
  photoType <- getPhotoTypeParam "type"

  case (image, serviceId, latitude, longitude, created, photoType) of
    (Left err, _, _, _, _, _) -> errorResponse err

    (_, Nothing, _, _, _, _)  ->
        errorResponse "serviceId is not specified or contains invalid value"

    (_, _, Left err, _, _, _) ->  errorResponse err

    (_, _, _, Left err, _, _) ->  errorResponse err

    (_, _, _, _, Nothing, _) ->
        errorResponse "created is not specified or contains invalid value"

    (_, _, _, _, _, Left err) ->  errorResponse err

    ( Right rawPhoto, Just serviceId', Right lat, Right lon, Just created', Right photoType') -> do
      filename <- saveFile rawPhoto serviceId' photoType' created'
      photoId <- savePhotoInfo driverId serviceId' lat lon created' photoType' filename
      okResponse $ T.pack $ show photoId

  where
    partHandler pInfo stream = do
      let fieldname   = BS.unpack $ partFieldName pInfo
          filename    = partFileName pInfo
          contentType = BS.unpack $ partContentType pInfo
      body <- BS.concat <$> Streams.toList stream
      return (fieldname, filename, contentType, body)


    saveFile rawPhoto serviceId photoType created = do
      cfg <- getSnapletUserConfig
      dirPrefix <- liftIO $ Config.require cfg "photo.directory"

      let (year, m, _) = toGregorian $ localDay $ zonedTimeToLocalTime created
          month = (if m < 10 then "0" else "") ++ show m
          dirname = dirPrefix </> show photoType </> show year </> month
          filenameTemplate = show serviceId ++ "_.jpg"

      liftIO $ createDirectoryIfMissing True dirname
      liftIO $ bracket (openBinaryTempFile dirname filenameTemplate)
                       (\(_, h) -> hClose h)
                       (\(filePath, h) ->
                            BS.hPut h rawPhoto >>
                            return (drop (length dirPrefix) filePath)
                       )


    savePhotoInfo driverId serviceId lat lon created photoType filename = do
      [Only photoId] :: [Only Int] <- query [sql|
        INSERT INTO driversPhotos
                    (driverId, serviceId, coord, created, photoType, filename)
             VALUES (?, ?, ST_SetSRID(ST_MakePoint(?, ?), 4326), ?, ?, ?)
          RETURNING id
      |] ( driverId, serviceId, lon, lat, created, photoType, filename)
      return photoId


getPhotos :: AppHandler ()
getPhotos = checkDriver $ \driverId -> do
  hasPhotos <- getPhotosFromDB driverId

  case hasPhotos of
    Right photos -> writeJSON $ object [ ("status", "ok")
                                      , ("message", toJSON photos)]
    Left err -> errorResponse err

  where
    getPhotosFromDB :: Int -> AppHandler (Either Text [PhotoInfo])
    getPhotosFromDB driverId = do
      hasService <- getIntParam "serviceId"

      case hasService of
        Nothing -> return $ Left $ T.pack "serviceId is not specified"

        Just serviceId -> do
          r <- getRequest

          photosInfo :: [PhotoInfo] <- query [sql|
            SELECT driverId
                 , serviceId
                 , ST_Y(coord)
                 , ST_X(coord)
                 , created
                 , photoType
                 , ? || '/' || id::text
              FROM driversPhotos
             WHERE driverId = ? and serviceId = ?
          |] ( rqURI r, driverId, serviceId)

          return $ Right photosInfo


getPhoto :: AppHandler ()
getPhoto = checkDriver $ \driverId -> do
  hasPhoto <- getIntParam "photoId"

  case hasPhoto of
    Nothing -> errorResponse "photo id is not specified"

    Just photoId -> do
      [Only filename] :: [Only (Maybe String)] <- query [sql|
        SELECT filename
          FROM driversPhotos
         WHERE driverId = ?
           AND id = ?
      |] ( driverId, photoId )

      case filename of
        Nothing -> errorResponse "invalid photo file name"
        Just "" -> errorResponse "empty photo file name"
        Just f  -> do
          cfg <- getSnapletUserConfig
          dirPrefix <- liftIO $ Config.require cfg "photo.directory"
          modifyResponse $ setContentType "image/jpeg"
          sendFile $ dirPrefix </> f
