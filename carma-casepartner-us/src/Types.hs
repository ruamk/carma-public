module Types
    ( LatestServices (..)
    , Location (..)
    , coords2Location
    ) where


import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Text    as T
import           GHC.Generics (Generic)


data LatestServices = All
                    | Closing
                    | Current


data Location = Location
    { latitude  :: Double
    , longitude :: Double
    } deriving (FromJSON, ToJSON, Generic, Show)


coords2Location :: T.Text -> Maybe Location
coords2Location coords =
    if T.null coords
    then Nothing
    else let [lon, lat] = map (read . T.unpack) $
                          T.split (==',') coords
         in Just $ Location lat lon
