{-# LANGUAGE LambdaCase #-}
module Carma.Utils.Snap
    ( bToString
    , getDateParam
    , getDateTimeParam
    , getDoubleParam
    , getIntParam
    , getJSONBody
    , getLatitudeParam
    , getLongitudeParam
    , getParamT
    , handleError
    , mkMap
    , parseMayParam
    , quote
    , readJSON
    , readJSONfromLBS
    , stringToB
    , withLens
    , writeJSON
    ) where


import qualified Control.Exception                as Ex
import           Control.Monad.State.Class
import           Data.Aeson                       as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.Attoparsec.ByteString.Lazy  (Result (..))
import qualified Data.Attoparsec.ByteString.Lazy  as Atto
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy             as L
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Time                        (Day)
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Format                 (defaultTimeLocale,
                                                   parseTimeM)
import           Data.Time.Format.ISO8601         (formatParseM, iso8601Format)
import           Data.Typeable

import           Snap


-- | Use the supplied parser to read a parameter from request. Fail
-- when the parameter is not present or can't be parsed.
parseMayParam :: MonadSnap m
              => Atto.Parser a
              -> ByteString
              -- ^ Parameter name.
              -> m (Maybe a)
parseMayParam parser k = do
  input <- fmap (Atto.parseOnly parser) <$> getParam k
  return $ case input of
             Just (Right p) -> Just p
             _              -> Nothing

getJSONBody :: Aeson.FromJSON v => Handler a b v
getJSONBody = readJSONfromLBS <$> readRequestBody (4 * 1024 * 1024)


data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Ex.Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f


readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"


readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> Ex.throw $ FromJSONError src err
    err -> Ex.throw $ AttoparsecError src (show err)


writeJSON :: ToJSON v => v -> Snap.Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode v


handleError :: MonadSnap m => Int -> m ()
handleError err = do
    modifyResponse $ setResponseCode err
    getResponse >>= finishWith


quote :: ByteString -> String
quote x = "'" ++ (T.unpack $ T.replace "'" "''" $ T.decodeUtf8 x) ++ "'"


-- | Convert UTF-8 encoded BS to Haskell string.
bToString :: ByteString -> String
bToString = T.unpack . T.decodeUtf8

-- | Inverse of 'bToString'.
stringToB :: String -> ByteString
stringToB = T.encodeUtf8 . T.pack


mkMap :: [Text] -> [[Maybe Text]] -> [Map Text Text]
mkMap fields = map $ Map.fromList . zip fields . map (fromMaybe "")


getParamT :: ByteString -> Handler a b (Maybe Text)
getParamT = fmap (fmap T.decodeUtf8) . getParam


getIntParam :: ByteString -> Handler a b (Maybe Int)
getIntParam name = do
  val <- getParam name
  return $ fst <$> (B.readInt =<< val)


getDoubleParam :: ByteString -> Handler a b (Maybe Double)
getDoubleParam = parseMayParam Atto8.double


getDoubleInRange :: ByteString
                 -> Double
                 -> Double
                 -> Handler a b (Either Text Double)
getDoubleInRange name minValue maxValue = do
  getDoubleParam name >>= \v ->
      return $ case v of
                  Nothing   -> Left $ T.concat [ T.pack $ B.unpack name
                                              , " is not specified"
                                              ]
                  Just val' -> if val' >= minValue && val' <= maxValue
                              then Right val'
                              else Left $ T.concat [ T.pack $ B.unpack name
                                                   , " is out of range "
                                                   , T.pack $ show minValue
                                                   , " .. "
                                                   , T.pack $ show maxValue
                                                   ]


getLatitudeParam :: ByteString -> Handler a b (Either Text Double)
getLatitudeParam name = getDoubleInRange name (-90.0) 90.0


getLongitudeParam :: ByteString -> Handler a b (Either Text Double)
getLongitudeParam name = getDoubleInRange name (-180.0) 180.0


getDateParam :: ByteString -> Handler a b (Maybe Day)
getDateParam name =
  getParam name >>= return . \case
                      Just d -> parseTimeM False defaultTimeLocale "%Y-%m-%d" $
                               B.unpack d
                      _      -> Nothing


getDateTimeParam :: ByteString -> Handler a b (Maybe UTCTime)
getDateTimeParam name =
  getParam name >>= return . \case
                      Just d -> formatParseM iso8601Format $ B.unpack d
                      _      -> Nothing


withLens :: MonadState s (Handler b v')
         => (s -> SnapletLens b v) -> Handler b v res
         -> Handler b v' res
withLens x = (gets x >>=) . flip withTop
