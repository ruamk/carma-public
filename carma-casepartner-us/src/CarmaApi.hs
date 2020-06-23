module CarmaApi
    where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Configurator (lookupDefault)
import qualified Data.Text as T
import           Network.HTTP (HeaderName(HdrCookie), mkHeader)
import           Snap.Snaplet (MonadSnaplet, getSnapletUserConfig)

import           Carma.HTTP
                 ( CarmaIO
                 , runCarma
                 , defaultCarmaOptions
                 , CarmaOptions(..)
                 )
import           Carma.HTTP.New (createInstance)
import           Carma.Utils.Operators ((&))
import           Carma.Model (IdentI, Ident(..))
import           Carma.Model.CaseComment as CaseComment
import           Data.Model.Patch as Patch


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

        
