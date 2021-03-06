{-|

Various string helpers used during export process.

-}

module Carma.SAGAI.Util
    ( padLeft
    , padRight
    , parseTimestamp
    , fvIdent
    )

where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Functor

import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Data.Model as Model


-- | Return string required to pad input up to provided length. Length
-- is calculated using bytes. If input is already not less than
-- required length, return empty string.
genericPad :: Int
           -- ^ Required result length.
           -> Char
           -- ^ Padding symbol.
           -> BS.ByteString
           -- ^ Input string.
           -> BS.ByteString
genericPad padLen pad input =
    if len < padLen
    then (B8.replicate (padLen - len) pad)
    else BS.empty
    where
      len = BS.length input


-- | Pad input using 'genericPad', keeping original string to the right.
padRight :: Int
         -- ^ Minimal string length.
         -> Char
         -- ^ Padding symbol.
         -> BS.ByteString
         -- ^ Input string.
         -> BS.ByteString
padRight padLen pad input = BS.append (genericPad padLen pad input) input


-- | Pad input using 'genericPad', keeping original string to the left.
padLeft :: Int
        -- ^ Minimal string length.
        -> Char
        -- ^ Padding symbol.
        -> BS.ByteString
        -- ^ Input string.
        -> BS.ByteString
padLeft padLen pad input = BS.append input (genericPad padLen pad input)


parseTimestamp :: B8.ByteString -> Maybe UTCTime
parseTimestamp = parseTime defaultTimeLocale "%s" . B8.unpack


-- | Convert an untyped field value to an Ident if it's a numeric
-- string.
fvIdent :: Model.Model m => BS.ByteString -> Maybe (Model.IdentI m)
fvIdent s = (Model.Ident . fst) <$> B8.readInt s
