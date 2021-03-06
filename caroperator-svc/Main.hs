module Main
    where

import           Control.Monad                        (unless, void, when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Aeson                           ((.=))
import qualified Data.Aeson                           as Aeson
import           Data.Char                            (isDigit)
import qualified Data.Configurator                    as Config
import           Data.Default.Class                   (Default (def))
import           Data.Maybe                           (isJust)
import           Data.Pool                            (Pool, createPool,
                                                       withResource)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Lazy                       as L
import           Data.Time                            (Day, ParseTime,
                                                       defaultTimeLocale,
                                                       getCurrentTime,
                                                       parseTimeM, toGregorian,
                                                       utctDay)
import qualified Database.PostgreSQL.Simple           as PG
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Text.Regex

import qualified System.Environment                   as Env
import           System.Posix.Syslog

import           Foreign.C.String
import           Network.HTTP.Types.Status            (status400)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger
import           System.Log.FastLogger
import           Web.Scotty


f :: LogStr -> String
f = T.unpack . T.decodeUtf8 . fromLogStr


main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      conf <- Config.load [Config.Required configPath]

      withSyslog prog [LogPID] User $ do
        withCStringLen ("Loading config from " ++ configPath) $
          syslog (Just User) Info

        carmaUsr  <- Config.require conf "carma.user"
        carmaPrg  <- Config.require conf "carma.subprogram"
        carmaTest <- Config.require conf "carma.test_mode"
        httpPort  <- Config.require conf "http.port"
        pgUri     <- Config.require conf "pg-conn-str"

        eltProgram      <- Config.require conf "program.elt"
        automamaProgram <- Config.require conf "program.automama"

        pgPool <- createPool (PG.connectPostgreSQL pgUri) PG.close
            1 -- number of distinct sub-pools
              -- time for which an unused resource is kept open
            20 -- seconds
            5 -- maximum number of resources to keep open

        reqLogger <- mkRequestLogger $ def
          { destination = Callback
              $ flip withCStringLen (syslog (Just User) Info) . f
          }
        withCStringLen ("Starting HTTP server on port " ++ show httpPort)
          $ syslog (Just User) Info
        scotty httpPort
          $ httpServer pgPool reqLogger
          $ SrvConfig carmaPrg carmaUsr carmaTest
                      eltProgram automamaProgram

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


data SrvConfig = SrvConfig
  { cfgSubProgram      :: Int
  , cfgCommitter       :: Int
  , cfgTestMode        :: Bool
  , cfgELTProgram      :: Int
  , cfgAutomamaProgram :: Int
  }


vinRx :: Regex
vinRx = mkRegexWithOpts "^[0-9A-HJ-NPR-Z]{17}$" False True


vinOk :: Text -> Bool
vinOk = isJust . matchRegex vinRx . T.unpack


phoneOk :: Text -> Bool
phoneOk phone = prefix `elem` ["+7"] &&
                T.length number == 10 &&
                T.all isDigit number
    where (prefix, number) = T.splitAt 2 phone


parseDate :: ParseTime a => Text -> L.Text -> ActionM a
parseDate date name =
    maybe (raise $ L.concat ["Invalid ", name, " format. Should be YYYY-MM-DD."]) pure
      $ parseTimeM True defaultTimeLocale "%F"
      $ T.unpack date


query :: (Control.Monad.IO.Class.MonadIO m, PG.FromRow r) =>
        Pool PG.Connection -> PG.Query -> m [r]
query pgPool q = liftIO $ withResource pgPool $ \c -> PG.query_ c q


queryParam :: (Control.Monad.IO.Class.MonadIO m, PG.ToRow q, PG.FromRow r) =>
             Pool PG.Connection -> PG.Query -> q -> m [r]
queryParam pgPool q p = liftIO $ withResource pgPool $ \c -> PG.query c q p


queryJSON :: Pool PG.Connection -> PG.Query -> ActionM ()
queryJSON pgPool q = do
  [[res]] <- query pgPool q
  json (res :: Aeson.Value)


queryParamJSON :: PG.ToRow q => Pool PG.Connection -> PG.Query -> q -> ActionM ()
queryParamJSON pgPool q p = do
  [[res]] <- queryParam pgPool q p
  json (res :: Aeson.Value)


handleErrors :: ActionM () -> ActionM ()
handleErrors f' = f' `rescue` \err -> do
  liftIO $ withCStringLen (L.unpack err) $ syslog (Just User) Error
  status status400 >> text err


httpServer :: Pool PG.Connection -> Middleware -> SrvConfig -> ScottyM ()
httpServer pgPool reqLogger cfg = do
  middleware reqLogger

  get "/CarMake" $ queryJSON pgPool
    [sql| with r as (select id, label from "CarMake")
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]

  -- for Mazda only
  get "/CarModel" $ queryJSON pgPool
    [sql| with r as (
            select id, label, parent as "carMake"
              from "CarModel"
              where parent = 28
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]

  get "/CarModel/:carMake" $ handleErrors $ do
    carMake <- param "carMake" :: ActionM Int
    queryParamJSON pgPool [sql| with r as (
                                  select id, label, parent as "carMake"
                                    from "CarModel"
                                   where parent = ?
                               )
                               select array_to_json(array_agg(row_to_json(r.*)))
                                 from r
                               |] $ PG.Only carMake

  -- for Mazda only
  get "/Dealer" $ queryJSON pgPool
    [sql| with r as (
            select id, name, addrs, phones
              from partnertbl p
              where isActive
                and isDealer
                and 28 = any(p.makes) -- make.id = 28 for Mazda
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]


  post "/Contract/Mazda" $ handleErrors $ do
    vin      <- param "vin"      :: ActionM Text
    carMake  <- param "carMake"  :: ActionM Int
    carModel <- param "carModel" :: ActionM Int
    sellDate <- param "sellDate" :: ActionM Text
    dealer   <- param "dealer"   :: ActionM Int

    unless (vinOk vin) $ raise "Invalid VIN format."
    -- TODO: implement vin checksum
    unless True  $ raise "Invalid VIN checksum."
    when (carMake /= 28) $ raise "Invalid carMake."

    (parsedDate :: Day) <-
      maybe (raise "Invalid sellDate format. Should be YYYY-MM-DD.") pure
        $ parseTimeM True defaultTimeLocale "%F"
        $ T.unpack sellDate

    [[modelOk, dealerOk]] <- liftIO $ withResource pgPool $ \c -> PG.query c
      [sql| select
        exists (select id from "CarModel" where parent = 28 and id = ?),
        exists (
          select id from partnertbl
          where id = ? and isActive and isDealer and 28 = any(makes))
      |] [carModel, dealer]

    unless modelOk  $ raise "Invalid carModel"
    unless dealerOk $ raise "Invalid dealer"

    void $ liftIO $ withResource pgPool $ \c -> PG.execute c
      [sql| insert into "Contract"
                ( vin, make, model, seller, subprogram
                , validsince, validuntil
                , dixi, committer)
              with p as (select
                  ? :: text    as vin,
                  ? :: int     as make,
                  ? :: int     as model,
                  ? :: int     as seller,
                  ? :: date    as validsince,
                  ? :: int     as subprogram,
                  ? :: boolean as testmode,
                  ? :: int     as committer)
              select
                  p.vin, p.make, p.model, p.seller, p.subprogram,
                  p.validsince,  p.validsince + interval '1 year',
                  p.testmode, p.committer
                from p
                where not exists (
                  select id from "Contract"
                    where vin = p.vin
                      and model = p.model
                      and seller = p.seller
                      and validsince = p.validsince)
      |]
      ( vin, carMake, carModel, dealer, parsedDate
      , cfgSubProgram cfg, not $ cfgTestMode cfg, cfgCommitter cfg
      )

    text "Ok"

  -- partner LT --

  get "/elt/subProgram" $ queryParamJSON pgPool
    [sql| with r as (
            select id, label
              from "SubProgram"
             where parent=?
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r;
    |] $ PG.Only $ cfgELTProgram cfg

  post "/elt/contract" $ handleErrors $ do
    let getCurrentYear = liftIO $ ((\(y,_,_) -> (fromInteger y) ::Int) .
                                   toGregorian .
                                   utctDay
                                  ) <$> getCurrentTime
        yearOk year thisYear = year >= thisYear - 100 && year <= thisYear

    subProgram <-             param "subProgram" :: ActionM Int
    name       <- T.strip <$> param "name"       :: ActionM Text
    phone      <- T.strip <$> param "phone"      :: ActionM Text
    vin        <- T.strip <$> param "vin"        :: ActionM Text
    cardNumber <- T.strip <$> param "cardNumber" :: ActionM Text
    carMake    <-             param "carMake"    :: ActionM Int
    carModel   <- T.strip <$> param "carModel"   :: ActionM Text
    makeYear   <-             param "makeYear"   :: ActionM Int
    buyDate    <- T.strip <$> param "buyDate"    :: ActionM Text
    validUntil <- T.strip <$> param "validUntil" :: ActionM Text

    [[subProgramOk]] <- queryParam pgPool
      [sql| select exists (
              select id from "SubProgram" where parent = ? and id = ?
            )
      |] (cfgELTProgram cfg, subProgram)

    unless subProgramOk $ raise "Invalid subProgram"
    when (T.null $ T.strip name) $ raise "Invalid name"
    unless (phoneOk phone) $ raise "Invalid phone"
    unless (vinOk vin) $ raise "Invalid vin format"
    realCardNumber <- do
      if T.null cardNumber
      then return Nothing
      else case reads (T.unpack cardNumber) :: [(Int, String)] of
             [(cn, "")] -> if cn > 0
                          then return $ Just cn
                          else raise "Invalid cardNumber"
             _ -> raise "Invalid cardNumber"

    [[makeOk]] <- queryParam pgPool
      [sql| select exists (
              select id from "CarMake" where id = ?
            )
      |] $ PG.Only carMake
    unless makeOk $ raise "Invalid carMake"

    thisYear <- getCurrentYear
    unless (yearOk makeYear thisYear) $ raise "Invalid makeYear"

    (parsedBuyDate :: Day) <- parseDate buyDate "buyDate"
    (parsedValidUntil :: Day) <- parseDate validUntil "validUntil"

    r <- queryParam pgPool
      [sql| insert into "Contract"
              ( subprogram, name, phone, vin
              , cardNumber, make, extra
              , makeYear, validSince, validUntil
              , dixi, committer
              )
              with p as
                (select ? :: int     as subprogram
                      , ? :: text    as name
                      , ? :: text    as phone
                      , ? :: text    as vin
                      , ? :: text    as cardnumber
                      , ? :: int     as make
                      , ? :: json    as extra
                      , ? :: int     as makeyear
                      , ? :: date    as validsince
                      , ? :: date    as validuntil
                      , ? :: boolean as testmode
                      , ? :: int     as committer
                )
              select p.subprogram, p.name, p.phone
                   , p.vin, p.cardnumber, p.make, p.extra
                   , p.makeyear, p.validsince, p.validuntil
                   , p.testmode, p.committer
                from p
               where not exists (
                 select id from "Contract"
                  where subprogram = p.subprogram
                    and name       = p.name
                    and phone      = p.phone
                    and vin        = p.vin
                    and cardnumber = p.cardnumber
                    and make       = p.make
                    and extra::json::text = p.extra::json::text
                    and makeyear   = p.makeyear
                    and validsince = p.validsince
                    and validuntil = p.validuntil
                 )
              returning id
      |] ( subProgram, name, phone, vin, realCardNumber
         , carMake, Aeson.object ["carModel" .= carModel], makeYear
         , parsedBuyDate, parsedValidUntil
         , not $ cfgTestMode cfg, cfgCommitter cfg
         )

    case r of
      [[contractId :: Int]] -> json contractId
      _                     -> raise "Duplicate"


  -- automama
  get "/automama/subProgram" $ queryParamJSON pgPool
    [sql| with r as (
            select id, label
              from "SubProgram"
             where parent=?
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r;
    |] $ PG.Only $ cfgAutomamaProgram cfg

  post "/automama/contract" $ handleErrors $ do
    let getCurrentYear = liftIO $ ((\(y,_,_) -> (fromInteger y) ::Int) .
                                   toGregorian .
                                   utctDay
                                  ) <$> getCurrentTime
        yearOk year thisYear = year >= thisYear - 100 && year <= thisYear

    subProgram <-             param "subProgram" :: ActionM Int
    name       <- T.strip <$> param "name"       :: ActionM Text
    phone      <- T.strip <$> param "phone"      :: ActionM Text
    vin        <- T.strip <$> param "vin"        :: ActionM Text
    cardNumber <- T.strip <$> param "cardNumber" :: ActionM Text
    carMake    <-             param "carMake"    :: ActionM Int
    carModel   <- T.strip <$> param "carModel"   :: ActionM Text
    makeYear   <-             param "makeYear"   :: ActionM Int
    buyDate    <- T.strip <$> param "buyDate"    :: ActionM Text
    validUntil <- T.strip <$> param "validUntil" :: ActionM Text
    comment    <- T.strip <$> param "comment"    :: ActionM Text

    [[subProgramOk]] <- queryParam pgPool
      [sql| select exists (
              select id from "SubProgram" where parent = ? and id = ?
            )
      |] (cfgAutomamaProgram cfg, subProgram)

    unless subProgramOk $ raise "Invalid subProgram"
    when (T.null $ T.strip name) $ raise "Invalid name"
    unless (phoneOk phone) $ raise "Invalid phone"
    unless (vinOk vin) $ raise "Invalid vin format"
    realCardNumber <- do
      if T.null cardNumber
      then return Nothing
      else case reads (T.unpack cardNumber) :: [(Int, String)] of
             [(cn, "")] -> if cn > 0
                          then return $ Just cn
                          else raise "Invalid cardNumber"
             _ -> raise "Invalid cardNumber"

    [[makeOk]] <- queryParam pgPool
      [sql| select exists (
              select id from "CarMake" where id = ?
            )
      |] $ PG.Only carMake
    unless makeOk $ raise "Invalid carMake"

    thisYear <- getCurrentYear
    unless (yearOk makeYear thisYear) $ raise "Invalid makeYear"

    (parsedBuyDate :: Day) <- parseDate buyDate "buyDate"
    (parsedValidUntil :: Day) <- parseDate validUntil "validUntil"

    r <- queryParam pgPool
      [sql| insert into "Contract"
              ( subprogram, name, phone, vin
              , cardNumber, make, extra
              , makeYear, validSince, validUntil
              , dixi, committer, comment
              )
              with p as
                (select ? :: int     as subprogram
                      , ? :: text    as name
                      , ? :: text    as phone
                      , ? :: text    as vin
                      , ? :: text    as cardnumber
                      , ? :: int     as make
                      , ? :: json    as extra
                      , ? :: int     as makeyear
                      , ? :: date    as validsince
                      , ? :: date    as validuntil
                      , ? :: boolean as testmode
                      , ? :: int     as committer
                      , ? :: text    as comment
                )
              select p.subprogram, p.name, p.phone
                   , p.vin, p.cardnumber, p.make, p.extra
                   , p.makeyear, p.validsince, p.validuntil
                   , p.testmode, p.committer, p.comment
                from p
               where not exists (
                 select id from "Contract"
                  where subprogram = p.subprogram
                    and name       = p.name
                    and phone      = p.phone
                    and vin        = p.vin
                    and cardnumber = p.cardnumber
                    and make       = p.make
                    and extra::json::text = p.extra::json::text
                    and makeyear   = p.makeyear
                    and validsince = p.validsince
                    and validuntil = p.validuntil
                 )
              returning id
      |] ( subProgram, name, phone, vin
         , realCardNumber, carMake, Aeson.object ["carModel" .= carModel]
         , makeYear, parsedBuyDate, parsedValidUntil
         , not $ cfgTestMode cfg, cfgCommitter cfg, comment
         )

    case r of
      [[contractId :: Int]] -> json contractId
      _                     -> raise "Duplicate"
