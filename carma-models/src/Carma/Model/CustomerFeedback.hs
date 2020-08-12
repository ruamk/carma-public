module Carma.Model.CustomerFeedback where

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Case (Case)
import Carma.Model.Service (Service)
import Carma.Model.Usermeta (Usermeta)
import Carma.Model.Satisfaction (Satisfaction)
import Carma.Model.Types()
import Carma.Model.PgTypes()

data CustomerFeedback = CustomerFeedback
  { ident     :: PK Int CustomerFeedback "Оценка качества обслуживания"
  , ctime     :: F UTCTime "ctime" "Дата создания"
  , caseId    :: F (IdentI Case) "caseId" "Номер кейса"
  , serviceId :: F (Maybe (IdentI Service)) "serviceId" "Номер услуги"
  , userId    :: F (IdentI Usermeta) "userId" "Кто проводил опрос"
  , value     :: F (IdentI Satisfaction) "value" "Результат опроса"
  , comment   :: F Text "comment" "Комментарий"
  } deriving Typeable

instance Model CustomerFeedback where
  type TableName CustomerFeedback = "CustomerFeedback"
  modelInfo = mkModelInfo CustomerFeedback ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
