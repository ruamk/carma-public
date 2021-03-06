{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch.Sql
  (create
  ,read, readPatch
  ,readMany, readManyWithFilter
  ,exists
  ,update
  ) where

import Prelude hiding (read)

import Control.Exception (try, SomeException)
import Data.Int (Int64)
import Data.String
import Text.Printf

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Types
import Data.Model.Patch


create
  :: forall m . Model m
  => Patch m -> Connection
  -> IO (Either SomeException (IdentI m))
create p c = try $ do
  let mInfo = modelInfo :: ModelInfo m
      (insFields, insTypes) = unzip $ getFieldsWTypes p
      q = printf "INSERT INTO %s (%s) VALUES (%s) RETURNING id"
        (show $ tableName mInfo)
        (T.unpack $ T.intercalate ", " insFields)
        (T.unpack $ T.intercalate ", " $ map ("?::" `T.append`) insTypes)

  [[res]] <- query c (fromString q) p
  return res


read :: forall m p. (FromRow (p m), Model m) =>
        IdentI m -> Connection -> IO (Either SomeException (p m))
read i c = try $ do
  let mInfo = modelInfo :: ModelInfo m
      fieldNames = map fd_name $ onlyDefaultFields $ modelFields mInfo
      q = printf "SELECT %s FROM %s WHERE id = ? LIMIT 2"
        (T.unpack $ T.intercalate ", " fieldNames)
        (show $ tableName mInfo)
  [res] <- query c (fromString q) [i]
  return res


{-# DEPRECATED readPatch "Add more types and use read instead" #-}
-- | Convenience wrapper for 'read' with legacy type.
readPatch :: forall m. Model m =>
             IdentI m -> Connection -> IO (Either SomeException (Patch m))
readPatch = read


exists :: forall m. Model m =>
          IdentI m -> Connection -> IO (Either SomeException Bool)
exists i c = try $ do
  let mInfo = modelInfo :: ModelInfo m
      q = printf "SELECT 1 FROM %s WHERE id = ?;"
          (show $ tableName mInfo)
  query c (fromString q) [i] >>=
    \case
      []         -> return False
      (Only e:_) -> return True
        where _ = e :: Int


readMany :: forall m . Model m => Int64 -> Int64 -> Connection -> IO [Patch m]
readMany lim off c = query_ c (fromString q)
  where
    mInfo = modelInfo :: ModelInfo m
    fieldNames = map fd_name $ onlyDefaultFields $ modelFields mInfo
    q = printf "SELECT %s FROM %s ORDER BY %s LIMIT %i OFFSET %i"
      (T.unpack $ T.intercalate ", " fieldNames)
      (show $ tableName mInfo)
      (T.unpack $ sortByFieldName mInfo)
      lim off

-- FIXME: supports only integer idents and text in filters
readManyWithFilter
  :: forall m . Model m
  => Int64 -> Int64
  -> [(Text,Text)]
  -> Connection -> IO [Patch m]
readManyWithFilter lim off params c = query c (fromString q) filterArgs
  where
    mInfo = modelInfo :: ModelInfo m
    fieldNames = map fd_name $ onlyDefaultFields $ modelFields mInfo
    q = printf "SELECT %s FROM %s WHERE %s ORDER BY %s LIMIT %i OFFSET %i"
      (T.unpack $ T.intercalate ", " fieldNames)
      (show $ tableName mInfo)
      filterPred (T.unpack $ sortByFieldName mInfo) lim off
    filterArgs = [val | (key,val) <- params, HashMap.member key (modelFieldsMap mInfo)]
    filterPred = T.unpack $ T.intercalate " AND "
      $ "TRUE"
      : [key <> " = ? :: " <> typ
        | (key,_) <- params
        , let Just fDesc = HashMap.lookup key (modelFieldsMap mInfo)
        , let typ = pgTypeName $ fd_pgType fDesc
        ]


update
  :: forall m . Model m
  => IdentI m -> Patch m -> Connection
  -> IO (Either SomeException Int64)
update (Ident i) p c
  = case updFields of
    [] -> return $ Right 1 -- do nothing if we have nothing to update
    _  -> try $ execute c (fromString q) p
  where
    updFields = map (\(n, t) -> T.concat [n, " = ?::", t]) $ getFieldsWTypes p
    mInfo = modelInfo :: ModelInfo m
    q = printf "UPDATE %s SET %s WHERE id = %d"
      (show $ tableName mInfo)
      (T.unpack $ T.intercalate ", " updFields)
      i

type Name = Text
type Type = Text

getFieldsWTypes :: forall m . Model m => Patch m -> [(Name, Type)]
getFieldsWTypes p =
  -- we use `map fst . HashMap.toList` instead of `HashMap.keys`
  -- just to be sure that `insFields` are in the same order as
  -- `ToRow (Patch m)` expects
  [(name, pgTypeName $ fd_pgType fd)
  | (name, _) <- HashMap.toList m
  , Just fd@FieldDesc{} <- [HashMap.lookup name (modelFieldsMap mInfo)]
  ]
  where
    m = untypedPatch p
    mInfo = modelInfo :: ModelInfo m
