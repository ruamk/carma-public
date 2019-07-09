{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

-- Fixes issue when record-fields aren't exported. Probably related to:
--   https://stackoverflow.com/questions/46357747/haddock-data-record-fields-names-not-being-generated
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | There's no response type here because by spec we're supposed to return
--   empty response body with 200 status in case everything is okay.
module Carma.EraGlonass.Types.EGRequestForServiceRequest
     ( EGRequestForServiceRequest         (..)
     , EGRequestForServiceRequestLocation (..)
     , EGRequestForServiceRequestVehicle  (..)

     , proof
     ) where

import           GHC.Generics

import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Swagger

import           Database.PostgreSQL.Simple.FromField
                   ( FromField (..)
                   , fromJSONField
                   )
import           Database.PostgreSQL.Simple.ToField
                   ( ToField (..)
                   , toJSONField
                   )
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.Sql (PersistFieldSql (sqlType))
import           Database.Persist.Class
                   ( PersistField (toPersistValue, fromPersistValue)
                   , fromPersistValueJSON
                   )

import           Data.Model (fieldName, fieldDesc)

import           Data.Model.Types
                   ( PgTypeable (pgTypeOf)
                   , PgType (PgType)
                   , DefaultFieldView (defaultFieldView)
                   , FieldView (..)
                   , FF
                   , fieldKindStr
                   )

import           Carma.Utils.Operators
import           Carma.Utils.StringyEnum
import           Carma.Utils.StringyEnum.Aeson
import           Carma.Utils.StringyEnum.SwaggerSchema
import           Carma.EraGlonass.Types.Helpers.Proof
import           Carma.EraGlonass.Types.Helpers.NonEmptyText
import           Carma.EraGlonass.Types.EGRequestId (EGRequestId)
import           Carma.EraGlonass.Types.EGDateTime (EGDateTime)
import           Carma.EraGlonass.Types.EGVin (EGVin)
import qualified Carma.EraGlonass.Types.EGLatLon as EGLatLon


proof :: ()
proof
  = proofThatTheTypeIsComplete (Proxy :: Proxy EGRequestForServiceRequest)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGRequestForServiceRequestLocation)
  ! proofThatTheTypeIsComplete
      (Proxy :: Proxy EGRequestForServiceRequestVehicle)


-- | First \"Request" is part of name of action and last ending \"Request" means
--   it's type of HTTP request body, not response.
data EGRequestForServiceRequest
   = EGRequestForServiceRequest
   { requestId :: EGRequestId
       -- ^ Unique identity of a request for service.

   , serviceCategoryId :: NonEmptyText
       -- ^ This a category of service which is chosen by EG's operator.
       --
       -- We probably don't need this value for our internal purposes.

   , statusCode :: EGRequestForServiceStatusCode
       -- ^ In this particular integration point it can only be @\"SENT"@.
       --
       -- Other integration points have the same field with different possible
       -- values.

   , statusTime :: EGDateTime
       -- ^ Last status update of this request for service.
       --
       -- I believe it may be earlier than time when request for service is
       -- delivered to us from EG.

   , ivsPhoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a terminal integrated to a vehicle.

   , fullName :: Maybe NonEmptyText
       -- ^ Surname, first name and middle name of a user of service (caller).

   , phoneNumber :: Maybe NonEmptyText
       -- ^ Phone number of a user of service (caller).

   , location :: EGRequestForServiceRequestLocation
       -- ^ Current coordinates of a user of service (caller).

   , deferTime :: Maybe EGDateTime
       -- ^ In case it's a delayed service - time when the service should be
       --   provided.

   , vehicle :: Maybe EGRequestForServiceRequestVehicle
       -- ^ Info about vehicle (VIN and optional plate number).

   , fccComment :: Maybe NonEmptyText
       -- ^ An optional comment by an operator of call center.

   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGRequestForServiceRequest where
  parseJSON :: forall t. t ~ EGRequestForServiceRequest => Value -> Parser t
  parseJSON =
    parseJSONWithOptionalNonEmptyTextFields'
      (Proxy :: Proxy '(t, "EGRequestForServiceRequest", 4))

instance PersistField EGRequestForServiceRequest where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue = fromPersistValueJSON

instance PersistFieldSql EGRequestForServiceRequest where
  sqlType Proxy = sqlType (Proxy :: Proxy Value)

instance PgTypeable EGRequestForServiceRequest where
  pgTypeOf _ = PgType "json" True

instance DefaultFieldView EGRequestForServiceRequest where
  defaultFieldView f = x where
    fieldToFieldKindProxy :: (m -> FF t nm desc a) -> Proxy a
    fieldToFieldKindProxy _ = Proxy
    x = FieldView
      { fv_name = fieldName f
      , fv_type = "JSON"
      , fv_canWrite = False
      , fv_meta =
          [ ("label", String $ fieldDesc f)
          , ("app",   String $ fieldKindStr $ fieldToFieldKindProxy f)
          ]
      }

instance FromField EGRequestForServiceRequest where
  fromField = fromJSONField

instance ToField EGRequestForServiceRequest where
  toField = toJSONField


data EGRequestForServiceRequestLocation
   = EGRequestForServiceRequestLocation
   { latitude    :: EGLatLon.EGLatitude
   , longitude   :: EGLatLon.EGLongitude
   , description :: Maybe NonEmptyText
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGRequestForServiceRequestLocation where
  parseJSON
    :: forall t. t ~ EGRequestForServiceRequestLocation => Value -> Parser t

  parseJSON =
    parseJSONWithOptionalNonEmptyTextFields'
      (Proxy :: Proxy '(t, "EGRequestForServiceRequestLocation", 1))


data EGRequestForServiceRequestVehicle
   = EGRequestForServiceRequestVehicle
   { vin :: EGVin
   , plateNumber :: Maybe NonEmptyText
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance FromJSON EGRequestForServiceRequestVehicle where
  parseJSON
    :: forall t. t ~ EGRequestForServiceRequestVehicle => Value -> Parser t

  parseJSON =
    parseJSONWithOptionalNonEmptyTextFields'
      (Proxy :: Proxy '(t, "EGRequestForServiceRequestVehicle", 1))


data EGRequestForServiceStatusCode = Sent
     deriving (Eq, Show, Enum, Bounded, Generic)

instance StringyEnum EGRequestForServiceStatusCode where
  toStringy Sent = "SENT"

instance ToJSON EGRequestForServiceStatusCode where
  toJSON = String . toStringy

instance FromJSON EGRequestForServiceStatusCode where
  parseJSON = parseStringyEnumJSON

instance ToSchema EGRequestForServiceStatusCode where
  declareNamedSchema = stringyEnumNamedSchema