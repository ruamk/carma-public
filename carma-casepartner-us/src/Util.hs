module Util
    ( -- * String helpers
      render
    , upCaseName
      -- * Time and date
    , projNow
      -- * Postgres helpers
    , ToRowList (..)
    , (:*) (..)
      -- * JSON responses
    , customOkResponse
    , errorResponse
    , okResponse
    ) where


import qualified Control.Exception                  as Ex
import           Data.Aeson                         as Aeson
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types


import           Application                        (AppHandler)
import           Carma.Utils.Snap                   (writeJSON)


data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Ex.Exception JSONParseException



upCaseName :: Text -> Text
upCaseName = T.unwords . map upCaseWord . T.words
  where
    upCaseWord w = T.concat [T.toUpper $ T.take 1 w, T.toLower $ T.drop 1 w]


-- | Simple templater (extracted from SMS module)
render :: Map Text Text
       -- ^ Context.
       -> Text
       -- ^ Template. Context keys @like_this@ are referenced
       -- @$like_this$@.
       -> Text
render varMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (expr, "")    -> [txt, evalVar expr]
        (expr, tpl'') -> txt : evalVar expr : loop (T.tail tpl'')

    evalVar v = Map.findWithDefault v v varMap


-- | Get current UNIX timestamp, round and apply a function to it,
-- then format the result as a bytestring.
projNow :: (Int -> Int) -> IO Text
projNow fn =
  T.pack . show . fn . round . utcTimeToPOSIXSeconds <$> getCurrentTime


-- | Works almost like '(:.)' for 'ToField' instances. Start with `()`
-- and append as many fields as needed:
--
-- > () :* f1 :* f2 :* f3
--
-- Initial `()` saves the type hassle.
data a :* b = a :* b deriving (Eq, Ord, Show, Read)

infixl 3 :*

instance (ToRow a, ToField b) => ToRow (a :* b) where
    toRow (a :* b) = toRow $ a :. Only b


-- | A list of 'ToRow' values with concatenating behavour of 'ToRow'.
newtype ToRowList a = ToRowList [a]

instance (ToRow a) => ToRow (ToRowList a) where
    toRow (ToRowList l) = concatMap toRow l



response :: Text -> Value -> Value
response s m = object [ ("status",  String s)
                      , ("message", m)
                      ]


okResponse :: Text -> AppHandler ()
okResponse message = writeJSON $ response "ok" $ String message


errorResponse :: Text -> AppHandler ()
errorResponse message = writeJSON $ response "error" $ String message


customOkResponse :: Value -> AppHandler ()
customOkResponse value = writeJSON $ response "ok" value


