{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.EraGlonass.Types
     ( AppContext (..)
     , EGCreateCallCardRequest (..)
     , EGCreateCallCardRequestGis (..)
     , EGCreateCallCardRequestVehicle (..)
     , EGPhoneNumber.EGPhoneNumber (EGPhoneNumber.EGPhoneNumber)
     , EGLatLon.EGLatitude, EGLatLon.toEGLatitude, EGLatLon.fromEGLatitude
     , EGLatLon.EGLongitude, EGLatLon.toEGLongitude, EGLatLon.fromEGLongitude
     , EGCallCardId.EGCallCardId (EGCallCardId.EGCallCardId)
     , EGCallerFullName.EGCallerFullName (EGCallerFullName.EGCallerFullName)
     , EGVin.EGVin (EGVin.EGVin)
     ) where

import           GHC.Generics (Generic)

import           Data.Text (Text, length)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger

import           Control.Concurrent.MVar (MVar)

import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.EraGlonass.RequestId (RequestId)
import qualified Carma.EraGlonass.Types.EGPhoneNumber as EGPhoneNumber
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon
import qualified Carma.EraGlonass.Types.EGCallCardId as EGCallCardId
import qualified Carma.EraGlonass.Types.EGCallerFullName as EGCallerFullName
import qualified Carma.EraGlonass.Types.EGVin as EGVin


data AppContext
   = AppContext
   { -- A bus to send log messages to
     loggerBus :: MVar LogMessage
   }


data EGCreateCallCardRequest
   = EGCreateCallCardRequest
   { requestId :: RequestId
       -- ^ Unique request identifier (required to answer)
       --   CaRMa field: TODO

   , cardIdCC :: EGCallCardId.EGCallCardId
       -- ^ Identity of "Call Card" (required to answer)
       --   Just a string, documentation says it's unsigned integer but in an
       --   example it is "597b53edf0f012e5e00d8a9a", after clarifying we know
       --   it is a free string value.
       --   CaRMa field: TODO

   , atPhoneNumber :: EGPhoneNumber.EGPhoneNumber
       -- ^ A contact phone number.
       --   A string from 1 to 18 chars of numbers which could be prefixed with
       --   '+'.
       --   CaRMa field: "contact_phone2"

   , lastTrustedLatitude :: EGLatLon.EGLatitude
       -- ^ An integer in range of -648000000 .. 648000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLongitude")

   , lastTrustedLongitude :: EGLatLon.EGLongitude
       -- ^ An integer in range of -324000000 .. 324000000
       --   CaRMa field: "caseAddress_coords" (alongwith "lastTrustedLatitude")

   , callerFullName :: EGCallerFullName.EGCallerFullName
       -- ^ A string of 50 chars.
       --   CaRMa field: "contact_name"

   , callerPhoneNumber :: EGPhoneNumber.EGPhoneNumber
       -- ^ A phone number of a customer (where "atPhoneNumber" could be some
       --   EG operator's phone I believe, TODO need to clarify this).
       --   A string from 1 to 18 chars of numbers which could be prefixed with
       --   '+'.
       --   CaRMa field: "contact_phone1"

   , locationDescription :: Text
       -- ^ String with max length of 180 symbols.
       --   CaRMa field: "caseAddress_comment"

   , vehicle :: EGCreateCallCardRequestVehicle
   , gis :: [EGCreateCallCardRequestGis]
   } deriving (Eq, Show, Generic, ToSchema)

instance FromJSON EGCreateCallCardRequest where
  parseJSON src = do
    parsed <- genericParseJSON defaultOptions src

    if Data.Text.length (locationDescription parsed) > 180
       then typeMismatch "EGCreateCallCardRequest.locationDescription" src
       else pure ()

    pure parsed


data EGCreateCallCardRequestGis
   = EGCreateCallCardRequestGis
   { regionName :: Text
       --   CaRMa field: "caseAddress_address"
   , settlementName :: Text
       --   CaRMa field: "caseAddress_address"
   , streetName :: Text
       --   CaRMa field: "caseAddress_address"
   , building :: Text
       -- ^ Number of a building as a string.
       --   CaRMa field: "caseAddress_address"
   } deriving (Eq, Show, Generic, ToSchema, FromJSON)


data EGCreateCallCardRequestVehicle
   = EGCreateCallCardRequestVehicle
   { vin :: EGVin.EGVin
       -- ^ A car's VIN ("Alphanumeric")
       --   We expect it to have length of 17 alphanumeric chars
       --   excluding "I", "O" and "Q" as declared in VIN spec.
       --   See also:
       --     https://en.wikipedia.org/wiki/Vehicle_identification_number
       --   CaRMa field: "contractIdentifier" and "car_vin" (just duplicate),
       --                see comments for these fields in "Case" model for
       --                details.

   , propulsion :: Text
       -- ^ Enum of:
       --     "HYDROGEN"
       --     "ELECTRICITY"
       --     "LPG" - Liquefied Petroleum Gas (propane)
       --     "LNG" - Liquefied Natural Gas
       --     "DIESEL"
       --     "GASOLINE"
       --   CaRMa field: "car_engine"
       --   TODO extend "Engine" model with missed engine types
       --   TODO implement NOT as string, also it is optional (Maybe)

   , color :: Text
       -- ^ A color of a car (string with max length of 50 symbols).
       --   CaRMa field: "car_color"
       --   TODO max length check

   , registrationNumber :: Text
       --   CaRMa field: "car_plateNum"
   } deriving (Eq, Show, Generic, ToSchema, FromJSON)
