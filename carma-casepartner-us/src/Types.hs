module Types
    ( DriverPhotoType (..)
    , LatestServices (..)
    , Location (..)
    , PhotoInfo (..)
    , coords2Location
    ) where


import           Data.Aeson                           (FromJSON, ToJSON,
                                                       genericToJSON, toJSON)
import           Data.Aeson.Types                     (constructorTagModifier,
                                                       defaultOptions,
                                                       fieldLabelModifier)
import qualified Data.ByteString.Char8                as BS
import           Data.Char                            (toLower)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time.Clock                      (UTCTime)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       ResultError (..),
                                                       returnError, typename)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           GHC.Generics                         (Generic)
import           Text.Read                            (readMaybe)



data LatestServices = All
                    | Closing
                    | Current


data Location = Location
    { latitude  :: Double
    , longitude :: Double
    } deriving (FromJSON, ToJSON, Generic, Show)


coords2Location :: Text -> Maybe Location
coords2Location coords =
    if T.null coords
    then Nothing
    else let [lon, lat] = map (read . T.unpack) $
                          T.split (==',') coords
         in Just $ Location lat lon


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
    { _driverId  :: Maybe Int
    , _serviceId :: Int
    , _latitude  :: Maybe Double
    , _longitude :: Maybe Double
    , _created   :: Maybe UTCTime
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
