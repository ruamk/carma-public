{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

SQL helpers used during VIN import process.

-}

module Carma.VIN.SQL

where

import qualified Blaze.ByteString.Builder.Char8 as BZ

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Data.Int
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.PostgreSQL.Simple hiding ( execute
                                                   , execute_
                                                   , query
                                                   , query_)
import qualified Database.PostgreSQL.Simple as PG ( execute
                                                  , execute_
                                                  , query
                                                  , query_)
import qualified Database.PostgreSQL.Simple.Copy as PG (copy)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Data.Model
import           Data.Model.Types
import           Carma.Model.Contract as C
import           Carma.Model.CarMake as CarMake
import           Carma.Model.CarModel as CarModel
import           Carma.Model.Partner
import           Carma.Model.Program
import           Carma.Model.SubProgram
import           Carma.Model.VinFormat

import           Carma.VIN.Base


-- | Text wrapper with a non-quoting 'ToField' instance.
newtype PlainText = PT InternalName

instance ToField PlainText where
    toField (PT i) = Plain $ BZ.fromText i


-- | Internal field name for a CSV column. It is neither external
-- column title, nor Contract field name.
type InternalName = T.Text


-- | Name of a field in Contract model.
newtype ContractFieldName = CN T.Text

instance ToField ContractFieldName where
    toField (CN i) = toField (PT i)


-- | Works almost like '(:.)' for 'ToField' instances. Start with `()`
-- and append as many fields as needed:
--
-- > () :* f1 :* f2 :* f3
--
-- Initial `()` saves the type hassle.
--
-- This is copied from carma's Snaplet.Geo
data a :* b = a :* b deriving (Eq, Ord, Show, Read)

infixl 3 :*

instance (ToRow a, ToField b) => ToRow (a :* b) where
    toRow (a :* b) = toRow $ a :. Only b


execute :: ToRow p => Query -> p -> Import Int64
execute q p = asks connection >>= \conn -> liftIO $ PG.execute conn q p


execute_ :: Query -> Import Int64
execute_ q = asks connection >>= \conn -> liftIO $ PG.execute_ conn q


query :: (FromRow r, ToRow p) => Query -> p -> Import [r]
query q p = asks connection >>= \conn -> liftIO $ PG.query conn q p


query_ :: (FromRow r) => Query -> Import [r]
query_ q = asks connection >>= \conn -> liftIO $ PG.query_ conn q


copy :: ToRow p => Query -> p -> Import ()
copy q p = asks connection >>= \conn -> liftIO $ PG.copy conn q p


errorsTitle :: Text
errorsTitle = "Ошибки"


minimalValidSince :: Text
minimalValidSince = "01-01-2009"


-- | Produce unique internal names from a CSV header.
mkInternalNames :: [ColumnTitle] -> [InternalName]
mkInternalNames columns =
    map (\(col, num) ->
             if col == errorsTitle
             then "errors"
             else fromString ("field" ++ show num)) $
    zip columns ([1..] :: [Int])


-- | Name of id field which maps proto entries to queue.
kid :: InternalName
kid = "id"


tKid :: PlainText
tKid = PT kid


cfn :: FieldI t n d => (Contract -> F t n d) -> ContractFieldName
cfn f= CN $ fieldName f


contractTable :: PlainText
contractTable = PT $ tableName
                (modelInfo :: ModelInfo Contract)


partnerTable :: PlainText
partnerTable = PT $ tableName
               (modelInfo :: ModelInfo Partner)


carMakeTable :: PlainText
carMakeTable = PT $ tableName
               (modelInfo :: ModelInfo CarMake)


carModelTable :: PlainText
carModelTable = PT $ tableName
                (modelInfo :: ModelInfo CarModel)


-- | First argument of @concat_ws@, quoted.
joinSymbol :: T.Text
joinSymbol = "' '"


sqlCast :: ContractFieldName -> T.Text -> (T.Text, ContractFieldName)
sqlCast cn@(CN n) t = (T.concat [n, "::", t], cn)


sqlCommas :: [T.Text] -> T.Text
sqlCommas = T.intercalate ","


getProgram :: Int
           -- ^ Subprogram id.
           -> Import [Only Int]
getProgram sid =
    query
    [sql|
     SELECT p.id FROM "?" p, "?" s
     WHERE p.id = s.parent AND s.id = ?;
     |] ( PT $ tableName (modelInfo :: ModelInfo Program)
        , PT $ tableName (modelInfo :: ModelInfo SubProgram)
        , sid)


-- | Loadable Contract field names.
contractFields :: [Text]
contractFields =
    map (\(FFAcc (FA c) _ _ _ _ _) -> fieldName c) vinFormatAccessors


-- | Create temporary pristine and proto tables for CSV data.
createCSVTables :: [InternalName]
                -> [ContractFieldName]
                -> Import ()
createCSVTables inames cnames =
    execute_ (mkProto "vinnie_pristine" inames) >>
    execute_ (mkProto "vinnie_proto" $ map (\(CN t) -> t) cnames) >>
    return ()
    where
      mkProto table names = fromString $ concat
                            [ "CREATE TEMPORARY TABLE "
                            , table
                            , " ("
                            , intercalate "," $
                              map (\n -> T.unpack n ++ " text") names ++
                              [T.unpack kid ++ " serial"]
                            , ");"
                            ]

-- | Delete temporary pristine and proto tables for CSV data.
deleteCSVTables :: Import ()
deleteCSVTables =
    execute_ "DROP TABLE IF EXISTS vinnie_pristine CASCADE;" >>
    execute_ "DROP TABLE IF EXISTS vinnie_proto CASCADE;" >>
    return ()

-- | Read CSV into pristine table.
copyPristineStart :: String -> [InternalName] -> Import ()
copyPristineStart enc inames =
  copy
  [sql|
   COPY vinnie_pristine (?)
   FROM STDIN (FORMAT CSV, HEADER 1, DELIMITER ';', ENCODING ?);
   |] (PT $ sqlCommas inames, enc)


-- | Initiate COPY FROM for report.
copyReportStart :: [InternalName] -> Import ()
copyReportStart inames =
    copy
    [sql|
     COPY (SELECT ?, array_to_string(q.errors, ';')
           FROM vinnie_pristine p, vinnie_queue q
           WHERE p.id = q.id AND q.errors IS NOT NULL
           ORDER BY p.id)
     TO STDIN (FORMAT CSV, FORCE_QUOTE *, DELIMITER ';');
     |] (Only $ PT $ sqlCommas $ delete "errors" inames)


-- | Copy pristine table data to proto.
pristineToProto :: [(InternalName, ContractFieldName)]
                -- ^ Internal names of loadable fields and target
                -- contract field names.
                -> Import Int64
pristineToProto names =
   execute
   [sql|
    INSERT INTO vinnie_proto (?)
    SELECT ?
    FROM vinnie_pristine;
    |] ( PT $ sqlCommas (kid : map (\(_, CN t) -> t) names)
       , PT $ sqlCommas (kid : map fst names))


protoUpdateWithFun :: ContractFieldName
                   -> Text
                   -- ^ Function name.
                   -> [Text]
                   -- ^ Function arguments, may only contain literals
                   -- or references to columns of vinnie_pristine.
                   -> Import Int64
protoUpdateWithFun cname fun args =
    execute
    [sql|
     UPDATE vinnie_proto SET ? = ?
     FROM vinnie_pristine
     WHERE vinnie_proto.? = vinnie_pristine.?;
     |] ( cname
        , PT $ T.concat [fun, "(", sqlCommas args, ")"]
        , tKid
        , tKid
        )


protoNullizeEmptyStrings :: ContractFieldName
                         -> Import Int64
protoNullizeEmptyStrings cname =
    execute
    [sql|
     UPDATE vinnie_proto SET ? = null
     WHERE vinnie_proto.? = '';
     |] ( cname
        , cname
        )


-- | Copy a pristine table column to proto table, trimming
-- leading/trailing whitespace.
protoTransfer :: InternalName -> ContractFieldName -> Import Int64
protoTransfer iname cname =
    execute
    [sql|
     UPDATE vinnie_proto SET ? = trim(both ' ' from ?)
     FROM vinnie_pristine
     WHERE vinnie_proto.? = vinnie_pristine.?;
     |] ( cname
        , PT iname
        , tKid
        , tKid
        )


-- | Copy a pristine table column to proto table, trimming whitespace
-- and translating characters.
protoTranslate :: InternalName
                   -> Text
                   -- ^ Source pattern.
                   -> Text
                   -- ^ Target pattern (its length must match that of
                   -- the source pattern).
                   -> ContractFieldName
                   -> Import Int64
protoTranslate iname from to cname =
    execute
    [sql|
     UPDATE vinnie_proto
     SET ? = translate(trim(both ' ' from ?), ?, ?)
     FROM vinnie_pristine
     WHERE vinnie_proto.? = vinnie_pristine.?;
     |] ( cname
        , PT iname
        , from
        , to
        , tKid
        , tKid
        )


-- | Clear field values which do not match a regular expression (case
-- insensitive).
protoCheckRegexp :: ContractFieldName
                 -> Text
                 -- ^ Regular expression.
                 -> Import Int64
protoCheckRegexp cname regexp =
    execute
    [sql|
     UPDATE vinnie_proto SET ? = null WHERE ? !~* ?;
     |] ( cname
        , cname
        , regexp)


-- | Clear bad dictionary element references.
protoDictCleanup :: InternalName
                 -> ContractFieldName
                 -> Text
                 -- ^ Dictionary table name.
                 -> Import Int64
protoDictCleanup iname cname dictTableName =
    execute
    [sql|
     CREATE OR REPLACE TEMPORARY VIEW dict_syn_cache AS
     SELECT DISTINCT
      -- TODO label/synonyms field names
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms)))) as label
      FROM "?";

     CREATE OR REPLACE TEMPORARY VIEW dict_labels AS
     SELECT
     lower(trim(both ' ' from ?)) as label,
     ? as id
     FROM vinnie_pristine;

     UPDATE vinnie_proto SET ?=null
     FROM vinnie_pristine s
     WHERE NOT EXISTS
     (SELECT 1 FROM dict_syn_cache, dict_labels
      WHERE vinnie_proto.? = dict_labels.id
      AND dict_labels.label = dict_syn_cache.label)
     AND vinnie_proto.? = s.?;
     |] ( PT dictTableName
        , PT iname
        , tKid
        , cname
        , tKid
        , tKid
        , tKid)


-- | Replace dictionary label references with dictionary element ids.
protoDictLookup :: InternalName
                 -> ContractFieldName
                 -> Text
                 -- ^ Dictionary table name.
                 -> Import Int64
protoDictLookup iname cname dictTableName =
    execute
    [sql|
     WITH dict AS
     (SELECT DISTINCT ON (label) id AS did,
      -- TODO label/synonyms field names
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms))))
       AS label
      FROM "?" ORDER BY label, did)
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict, vinnie_pristine s
     WHERE length(lower(trim(both ' ' from ?))) > 0
     AND dict.label=lower(trim(both ' ' from ?))
     AND vinnie_proto.? = s.?;
     |] ( PT dictTableName
        , cname
        , PT iname
        , PT iname
        , tKid
        , tKid)


-- | Clear bad partner references.
protoPartnerCleanup :: [InternalName]
                    -> ContractFieldName
                    -> Import Int64
protoPartnerCleanup inames cname =
    execute
    [sql|
     CREATE OR REPLACE TEMPORARY VIEW partner_syn_cache AS
     SELECT DISTINCT
     -- TODO name/code/synonyms field names
     lower(trim(both ' ' from (unnest(ARRAY[name, code] || synonyms)))) as label
     FROM "?";

     CREATE OR REPLACE TEMPORARY VIEW partner_labels AS
     SELECT
     lower(trim(both ' ' from unnest(ARRAY[?]))) as label,
     ? as id
     FROM vinnie_pristine;

     UPDATE vinnie_proto SET ?=null
     FROM vinnie_pristine s
     WHERE NOT EXISTS
     (SELECT 1 FROM partner_syn_cache, partner_labels
      WHERE vinnie_proto.? = partner_labels.id
      AND partner_labels.label = partner_syn_cache.label)
     AND vinnie_proto.? = s.?;
     |] (()
         :* partnerTable
         :* (PT $ sqlCommas inames)
         :* tKid
         :* cname
         :* tKid :* tKid :* tKid)


-- | Replace partner label/code references with partner ids.
protoPartnerLookup :: InternalName
                   -> ContractFieldName
                   -> Import Int64
protoPartnerLookup iname cname = do
    let params = (()
                  :* partnerTable
                  :* cname
                  :* PT iname
                  :* PT iname
                  :* tKid :* tKid)
    -- TODO We can just use one query here with unnest + WITH
    -- ORDINALITY if we had PG 9.5.x
    execute
      [sql|
       WITH dict AS
       (SELECT DISTINCT ON (label) id AS did,
        lower(trim(both ' ' from (unnest(ARRAY[name]))))
         AS label
        FROM "?" ORDER BY label, did)
       UPDATE vinnie_proto SET ? = dict.did
       FROM dict, vinnie_pristine s
       WHERE length(lower(trim(both ' ' from ?))) > 0
       AND dict.label=lower(trim(both ' ' from ?))
       AND vinnie_proto.? = s.?;
       |] params

    execute
      [sql|
       WITH dict AS
       (SELECT DISTINCT ON (label) id AS did,
        lower(trim(both ' ' from (unnest(synonyms))))
         AS label
        FROM "?" ORDER BY label, did)
       UPDATE vinnie_proto SET ? = dict.did
       FROM dict, vinnie_pristine s
       WHERE length(lower(trim(both ' ' from ?))) > 0
       AND dict.label=lower(trim(both ' ' from ?))
       AND vinnie_proto.? = s.?;
       |] params

    execute
      [sql|
       WITH dict AS
       (SELECT DISTINCT ON (label) id AS did,
        lower(trim(both ' ' from (unnest(ARRAY[code]))))
         AS label
        FROM "?" ORDER BY label, did)
       UPDATE vinnie_proto SET ? = dict.did
       FROM dict, vinnie_pristine s
       WHERE length(lower(trim(both ' ' from ?))) > 0
       AND dict.label=lower(trim(both ' ' from ?))
       AND vinnie_proto.? = s.?;
       |] params


-- | Replace subprogram label references with subprogram ids. Only
-- children of the provided program are selected. If no reference
-- found, set to null.
protoSubprogramLookup :: Int
                      -- ^ Program id.
                      -> InternalName
                      -> ContractFieldName
                      -> Import Int64
protoSubprogramLookup pid iname cname =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=null
     FROM vinnie_pristine s
     WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
      lower(trim(both ' ' from (unnest(ARRAY[sub.label] || synonyms))))
      FROM "?" sub, "?" p WHERE sub.parent = p.id AND p.id = ?)
     AND vinnie_proto.? = s.?;

     -- TODO label/parent fields
     WITH dict AS
     (SELECT DISTINCT sub.id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[sub.label] || synonyms)))) AS label
      FROM "?" sub, "?" p WHERE sub.parent = p.id AND p.id = ? ORDER BY did)
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict, vinnie_pristine s
     WHERE length(lower(trim(both ' ' from ?))) > 0
     AND dict.label=lower(trim(both ' ' from ?))
     AND vinnie_proto.? = s.?;
     |] (()
         :* cname
         :* PT iname
         :* subProgramTable
         :* programTable
         :* pid
         :* tKid :* tKid

         :* subProgramTable
         :* programTable
         :* pid
         :* cname
         :* PT iname
         :* PT iname
         :* tKid :* tKid)
        where
          programTable = PT $ tableName (modelInfo :: ModelInfo Program)
          subProgramTable = PT $ tableName (modelInfo :: ModelInfo SubProgram)


-- | TODO Move @lower(trim(both ' ' from $1))@ to functions (when the
-- performance issue is resolved).
installFunctions :: Import Int64
installFunctions =
    execute_
    [sql|
     CREATE OR REPLACE FUNCTION pg_temp.dateordead(text) RETURNS text AS $$
     DECLARE x DATE;
     BEGIN
         -- check format 'dd.mm.yyyy'
         IF $1 !~ '^(?:0[1-9]|[12][0-9]|3[0-1])\.(?:0[1-9]|1[012])\.(?:19|20)\d{2}$' THEN
             RETURN null;
         END IF;
         x = $1::DATE;
         RETURN $1;
     EXCEPTION WHEN others THEN
         RETURN null;
     END;
     $$ LANGUAGE plpgsql;

     CREATE OR REPLACE FUNCTION pg_temp.numordead(text) RETURNS text AS $$
     DECLARE x NUMERIC;
     BEGIN
         x = $1::NUMERIC;
         RETURN $1;
     EXCEPTION WHEN others THEN
         RETURN null;
     END;
     $$ LANGUAGE plpgsql;

     CREATE OR REPLACE FUNCTION pg_temp.mileageordead(text) RETURNS int AS $$
     DECLARE x NUMERIC;
     BEGIN
         IF $1 IS NULL THEN
             RETURN null;
         END IF;

         IF $1 LIKE '0%' THEN
             RETURN null;
         END IF;

         x = $1::INTEGER;
         IF (x > 0) AND (x <= 1999999) THEN
             RETURN $1;
         ELSE
             RETURN null;
         END IF;
     EXCEPTION WHEN others THEN
         RETURN null;
     END;
     $$ LANGUAGE plpgsql;

     CREATE OR REPLACE FUNCTION pg_temp.phoneordead(text) RETURNS text AS $$
     BEGIN
         IF $1 IS NULL THEN
             RETURN null;
         END IF;

         IF $1 ~ '^\+7\d{10}$' THEN
             RETURN $1;
         END IF;

         IF $1 ~ '^8\d{10}$' THEN
             RETURN '+7' || substring($1, 2);
         END IF;

         RETURN null;
     END;
     $$ LANGUAGE plpgsql;
     |] >>

    execute
    [sql|
     CREATE OR REPLACE FUNCTION pg_temp.checkmakemodel(integer, integer)
     RETURNS BOOLEAN AS $$
     DECLARE
         makeId integer = $1;
         modelId integer = $2;
     BEGIN
         PERFORM *
         FROM "?" AS maker, "?" AS model
         WHERE ? = maker.id AND
               makeId = maker.id AND
               modelId = model.id;

         RETURN FOUND;
     END;
     $$ LANGUAGE plpgsql;
     |] ( carMakeTable, carModelTable
        , PT $ fieldName CarModel.parent)


-- | Create a purgatory for new contracts with schema identical to
-- Contract model table.
createQueueTable :: Import Int64
createQueueTable =
    execute
    [sql|
     CREATE TEMPORARY TABLE vinnie_queue
     AS (SELECT * FROM "?" WHERE 'f');
     ALTER TABLE vinnie_queue ADD COLUMN errors text[];

     -- subprogram is filled last in setSpecialDefaults, thus we bind
     -- triggers to subprogram field updates.
     CREATE TRIGGER "vinnie_queue_validUntil_update" BEFORE UPDATE OF ?
     ON vinnie_queue
     FOR EACH ROW
     WHEN (NEW.? IS NOT NULL
           AND NEW.? IS NOT NULL
           AND NEW.? IS NULL)
     EXECUTE PROCEDURE fillValidUntil();

     -- subprogram may also be recognized prior to
     -- vinnie_proto-vinnie_queue transfer from a file column, thus an
     -- insert trigger is also required
     CREATE TRIGGER "vinnie_queue_validUntil_insert" BEFORE INSERT
     ON vinnie_queue
     FOR EACH ROW
     WHEN (NEW.? IS NOT NULL
           AND NEW.? IS NOT NULL
           AND NEW.? IS NULL)
     EXECUTE PROCEDURE fillValidUntil();

     CREATE TRIGGER "vinnie_queue_checkPeriod_update" BEFORE UPDATE OF ?
     ON vinnie_queue
     FOR EACH ROW
     WHEN (NEW.? IS NOT NULL
           AND NEW.? IS NULL)
     EXECUTE PROCEDURE fillCheckPeriod();

     CREATE TRIGGER "vinnie_queue_checkPeriod_insert" BEFORE INSERT
     ON vinnie_queue
     FOR EACH ROW
     WHEN (NEW.? IS NOT NULL
           AND NEW.? IS NULL)
     EXECUTE PROCEDURE fillCheckPeriod();

     CREATE TRIGGER "vinnie_queue_fts_trigger" BEFORE INSERT OR UPDATE
     ON vinnie_queue
     FOR EACH ROW
     EXECUTE PROCEDURE "Contract_fts_key_update"();
     |] (()
         :* contractTable
         :* cfn C.subprogram
         :* cfn C.validSince :* cfn C.subprogram :* cfn C.validUntil
         :* cfn C.validSince :* cfn C.subprogram :* cfn C.validUntil
         :* cfn C.subprogram
         :* cfn C.subprogram :* cfn C.checkPeriod
         :* cfn C.subprogram :* cfn C.checkPeriod)

-- | Create a purgatory for new contracts with schema identical to
-- Contract model table.
deleteQueueTable :: Import ()
deleteQueueTable =
    execute_ "DROP TABLE IF EXISTS vinnie_queue CASCADE;" >>
    return ()

-- | Set committer and subprogram (if not previously set) for
-- contracts in queue.
setSpecialDefaults :: Int
                   -- ^ Committer.
                   -> Maybe Int
                   -- ^ Subprogram.
                   -> Bool
                   -- ^ Contracts are loaded from ARC.
                   -> String
                   -- ^ Source file name.
                   -> Import Int64
setSpecialDefaults cid sid fromArcVal filename =
    execute
    [sql|
     UPDATE vinnie_queue SET ? = ?;
     UPDATE vinnie_queue SET ? = 't';
     UPDATE vinnie_queue SET ? = ? WHERE ? IS NULL;
     UPDATE vinnie_queue SET ? = ?;
     UPDATE vinnie_queue SET ? = ?;
     |] (cfn C.committer, cid,
         cfn C.dixi,
         cfn C.subprogram, sid, cfn C.subprogram,
         cfn C.fromArc, fromArcVal,
         cfn C.sourceFile, filename)


-- | Transfer from proto table to queue table, casting text values
-- from proto table to actual types used by Contract model.
protoToQueue :: [(Text, ContractFieldName)]
             -- ^ Internal column names of proto table with type
             -- casting specifiers (@mileage::int@), mapped to
             -- Loadable Contract columns (@mileage@).
             -> Import Int64
protoToQueue names =
    execute
    [sql|
     INSERT INTO vinnie_queue (?)
     SELECT ?
     FROM vinnie_proto;
     |] ( PT $ sqlCommas (kid : map (\(_, CN t) -> t) names)
        , PT $ sqlCommas (kid : map fst names))


-- | Set default values for a column in queue table. Parameters: field
-- name, default value, field name again.
setQueueDefaults :: Query
setQueueDefaults =
    [sql|
     UPDATE vinnie_queue SET ? = ?
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Add error to every row with missing identifier fields.
markMissingIdentifiers :: Import Int64
markMissingIdentifiers =
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE length(lower(trim(both ' ' from concat(?)))) = 0;
     |] ( NoIdentifiers
        , PT $ sqlCommas identifierNames)


-- | Add error to every row where a provided field is empty (used to
-- mark empty required fields). Parameters: error message, field name.
markEmptyRequired :: Query
markEmptyRequired =
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Add error to every row where combination make/model is invalid.
markInvalidMakeModel :: Import Int64
markInvalidMakeModel =
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE make IS NOT NULL AND
           model IS NOT NULL AND
           NOT pg_temp.checkmakemodel(make, model);
    |] (Only InvalidMakeModel)


-- | Add error to every row where invalid date in columns validSince
-- | and validUntil.
markInvalidDates :: Import Int64
markInvalidDates =
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE validSince IS NOT NULL AND
           validUntil IS NOT NULL AND
           validSince > validUntil;
    |] (Only $ ValidSinceGreaterValidUntil
                 (fieldDesc C.validSince)
                 (fieldDesc C.validUntil))
    >>
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE validSince < ?::date;
    |] ( ValidSinceLessMinimum (fieldDesc C.validSince) minimalValidSince
       , minimalValidSince)
    >>
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE validSince > date(now());
    |] (Only $ ValidSinceGreaterNow $ fieldDesc C.validSince)


-- | Calculate how many erroneous rows are in queue table.
countErrors :: Import Int64
countErrors = do
  [Only bad] <-
      query_ [sql|SELECT count(1) FROM vinnie_queue WHERE errors IS NOT NULL;|]
  return bad


deferConstraints :: Connection -> IO Int64
deferConstraints conn =
  PG.execute_ conn
  [sql|
   SET CONSTRAINTS ALL DEFERRED;
   |]


-- | Delete all rows from the queue which are already present in
-- Contract table.
--
-- IO monad to be try-friendly.
deleteDupes :: Connection -> IO Int64
deleteDupes conn =
    PG.execute conn
    [sql|
     DELETE FROM vinnie_queue q USING "?" c
       WHERE ?
         AND c.fts_key = q.fts_key
         AND c.isactive
         AND c.dixi
     |] ( contractTable
        , PT $ T.intercalate " AND " $
          map (\f -> T.concat [ "((q.", f, " = ","c.", f
                              , ") OR ("
                              , "q.", f, " IS NULL AND c.", f, " IS NULL))"])
          contractFields
        )


-- | Transfer all contracts w/o errors from queue to live Contract
-- table.
--
-- IO monad to be try-friendly.
transferContracts :: Connection -> IO Int64
transferContracts conn =
    PG.execute conn
    [sql|
     INSERT INTO "?" (?)
     SELECT DISTINCT ?
     FROM vinnie_queue WHERE errors IS NULL;
     |] ( contractTable
        , PT $ sqlCommas $
          fieldName C.fromArc:
          fieldName C.committer:
          fieldName C.dixi:
          fieldName C.sourceFile:
          contractFields
        , PT $ sqlCommas $
          fieldName C.fromArc:
          fieldName C.committer:
          fieldName C.dixi:
          fieldName C.sourceFile:
          contractFields)


data RowError = EmptyRequired Text
              | NoIdentifiers
              | NoSubprogram
              | InvalidMakeModel
              | ValidSinceGreaterValidUntil Text Text
              | ValidSinceLessMinimum Text Text
              | ValidSinceGreaterNow Text
                deriving Show

instance ToField RowError where
    toField (EmptyRequired t) =
        toField $ T.concat ["Обязательное поле «", t, "» не распознано"]
    toField (NoIdentifiers) =
        toField $ T.concat ["Ни одно из полей-идентификаторов не распознано"]
    toField NoSubprogram =
        toField $ T.concat ["Подпрограмма не распознана"]
    toField InvalidMakeModel =
        toField $ T.concat ["Комбинация марка-модель указана не верно"]
    toField (ValidSinceGreaterValidUntil vs vu) =
        toField $ T.concat [vs, " больше ", vu]
    toField (ValidSinceLessMinimum vs minimim) =
        toField $ T.concat [vs, " меньше ", minimim]
    toField (ValidSinceGreaterNow vs) =
        toField $ T.concat [vs, " больше текущей даты"]
