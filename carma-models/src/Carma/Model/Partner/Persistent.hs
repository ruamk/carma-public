{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | @Partner@ persistent model.
module Carma.Model.Partner.Persistent
    where

import           Data.Aeson                       (Value)
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)
import           Data.Typeable

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance

import           Carma.Model.CarMake.Persistent   (CarMakeId)
import           Carma.Model.City.Persistent      (CityId)
import           Carma.Model.TaxScheme.Persistent (TaxSchemeId)
import           Carma.Model.Types                (Coords)


-- | @Partner@ persistent model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Partner json sql=partnertbl
  isActive  Bool sql=isactive
  isDealer  Bool sql=isdealer
  isPartner Bool sql=ispartner
  isMobile  Bool sql=ismobile
  isFree    Bool sql=isfree

  name Text sql=name
  synonyms [Text] sql=synonyms
  code Text Maybe sql=code
  city CityId Maybe sql=city
  makes [CarMakeId] sql=makes
  services Value sql=services
  phones Value sql=phones
  coords Coords Maybe sql=coords
  addrs Value sql=addrs
  emails Value sql=emails
  personInCharge Text Maybe sql=personincharge
  taxScheme TaxSchemeId Maybe sql=taxscheme
  legalName Text Maybe sql=legalname
  inn Text Maybe sql=inn
  code1c Text Maybe sql=code1c
  isKpiEnabled Bool sql=iskpienabled
  isPayBackConfirmed Bool sql=ispaybackconfirmed
  foreignIdent Text Maybe sql=foreignident
  mtime UTCTime sql=mtime
  comment Text sql=comment

  deriving Typeable Show
|]
