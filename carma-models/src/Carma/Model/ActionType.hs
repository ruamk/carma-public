{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.ActionType where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Data.Model.TH

import Carma.Model.PgTypes()

data ActionType = ActionType
  { ident
    :: PK Int ActionType "Действие"
  , label
    :: F Text "label" "Тип действия"
  , desc
    :: F Text "description" "Описание"
  , priority
    :: F Int "priority" "Приоритет"
  , maxSeconds
    :: F Int "maxSeconds" "Регламентное время выполнения, с"
  } deriving Typeable

mkIdents [t|ActionType|]
 [ ("orderService", 1)
 , ("orderServiceAnalyst", 2)
 , ("tellClient", 3)
 , ("checkStatus", 4)
 , ("needPartner", 5)
 , ("checkEndOfService", 6)
 , ("closeCase", 7)
 , ("getDealerInfo", 8)
 , ("cancelService", 9)
 , ("makerApproval", 10)
 , ("tellMakerDeclined", 11)
 , ("addBill", 12)
 , ("billmanNeedInfo", 13)
 , ("headCheck", 14)
 , ("directorCheck", 15)
 , ("accountCheck", 16)
 , ("analystCheck", 17)
 , ("complaintResolution", 18)
 , ("tellMeMore", 19)
 , ("callMeMaybe", 20)
 , ("checkDispatchTime", 21)
 , ("accident", 22)
 , ("confirmPartnerDelay", 23)
 , ("rushOrder", 24)
 , ("call", 100)
 ]

instance Model ActionType where
  type TableName ActionType = "ActionType"
  idents = Carma.Model.ActionType.idents
  modelInfo = mkModelInfo ActionType ident
  modelView = \case
    "" -> Just $ modifyView defaultView
          [ infoText "actpriority" priority
          ]
    _  -> Nothing
