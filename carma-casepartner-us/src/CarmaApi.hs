module CarmaApi
    where

import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Configurator                    (lookupDefault)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time.Clock                      (getCurrentTime)
import           Network.HTTP                         (HeaderName (HdrCookie),
                                                       mkHeader)
import           Snap.Snaplet                         (MonadSnaplet,
                                                       getSnapletUserConfig)

import           Carma.HTTP                           (CarmaIO,
                                                       CarmaOptions (..),
                                                       defaultCarmaOptions,
                                                       runCarma)
import           Carma.HTTP.New                       (createInstance,
                                                       updateInstance)
import           Carma.Model                          (Ident (..), IdentI)
import           Carma.Model.Action                   as Action
import           Carma.Model.ActionResult             as ActionResult
import           Carma.Model.CaseComment              as CaseComment
import           Carma.Model.PartnerDelay             as PartnerDelay
import           Carma.Model.PartnerDelay.Confirmed   as Confirmed
import           Carma.Model.PartnerDelay.Exceptional as Exceptional
import           Carma.Model.PartnerDelay.Notified    as Notified
import           Carma.Model.Service                  as Service
import           Carma.Model.ServiceStatus            as ServiceStatus
import           Carma.Utils.Operators                ((&))
import           Data.Model.Patch                     as Patch


defaultHostname :: String
defaultHostname = "127.0.0.1"

defaultPort :: Int
defaultPort = 8000


carma :: (MonadIO (m b v), MonadSnaplet m) => String -> CarmaIO b1 -> m b v b1
carma cookie action = do
  cfg <- getSnapletUserConfig

  (hostname :: String) <- liftIO $ lookupDefault defaultHostname cfg "carma.host"
  port <- liftIO $ lookupDefault defaultPort cfg "carma.port"

  let carmaOptions = defaultCarmaOptions
                         { carmaHost = hostname
                         , carmaPort = port
                         , carmaHeaders = [ mkHeader HdrCookie cookie ]
                         }
  liftIO $ runCarma carmaOptions action


addComment
    :: (MonadSnaplet m, MonadIO (m b v))
    => String
    -> Int
    -> String
    -> m b v ( IdentI CaseComment, Patch CaseComment )
addComment cookie caseId comment = carma cookie $ createInstance cc
    where cc = Patch.empty
             & Patch.put CaseComment.caseId (Ident caseId)
             & Patch.put CaseComment.comment (T.pack comment)


addPartnerDelay
    :: (MonadSnaplet m, MonadIO (m b v))
    => String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe Text
    -> m b v ( IdentI PartnerDelay, Patch PartnerDelay )
addPartnerDelay cookie caseId serviceId ownerId partnerId minutes delayReason delayReasonComment =
    carma cookie $ createInstance pd
    where pd = Patch.empty
             & Patch.put PartnerDelay.caseId (Ident caseId)
             & Patch.put PartnerDelay.serviceId (Ident serviceId)
             & Patch.put PartnerDelay.owner (Ident ownerId)
             & Patch.put PartnerDelay.partnerId (Ident partnerId)
             & Patch.put PartnerDelay.delayConfirmed Confirmed.needConfirmation
             & Patch.put PartnerDelay.exceptional Exceptional.no
             & Patch.put PartnerDelay.notified Notified.yes
             & Patch.put PartnerDelay.delayMinutes minutes
             & Patch.put PartnerDelay.delayReason (Ident delayReason)
             & Patch.put PartnerDelay.delayReasonComment
                         delayReasonComment



-- | Изменение результата действия "Контроль доезда помощи"
-- | на "Услуга в процессе оказания"

serviceInProgress
    :: ( MonadIO (m b v)
      , MonadSnaplet m
      )
    => String
    -> IdentI Action
    -> m b v (Patch Action)
serviceInProgress cookie actionId = do
  carma cookie $
      updateInstance actionId
                     ( Patch.empty
                     & Patch.put Action.result (Just ActionResult.serviceInProgress)
                     )


serviceClosed
    :: ( MonadIO (m b v )
      , MonadSnaplet m
      )
    => String
    -> IdentI Service
    -> IdentI Action
    -> m b v (Patch Service)
serviceClosed cookie serviceId closeActionId{-checkActionId-} = do
  void $ carma cookie
       $ updateInstance closeActionId
             ( Patch.empty
             & Patch.put Action.result (Just ActionResult.caseClosed)
             )
  carma cookie $ updateInstance serviceId
                     ( Patch.empty
                     & Patch.put Service.status ServiceStatus.closed
                     )

updateFactServiceStartTime
    :: ( MonadIO (m b v)
      , MonadSnaplet m
      )
    => String
    -> IdentI Service
    -> m b v (Patch Service)
updateFactServiceStartTime cookie serviceId = do
  now <- liftIO getCurrentTime
  carma cookie $
        updateInstance serviceId
                       ( Patch.empty
                       & Patch.put Service.times_factServiceStart (Just now)
                       )


partnerIsDone
    :: (MonadIO (m b v)
      , MonadSnaplet m
      )
    => String
    -> IdentI Service
    -> m b v (Patch Service)
partnerIsDone cookie serviceId = carma cookie $
  updateInstance serviceId
                 ( Patch.empty
                 & Patch.put Service.partnerIsDone True
                 )
