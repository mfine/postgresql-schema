-- |
-- Module:      Database.PostgreSQL.Schema
-- Copyright:   (c) 2015 Mark Fine
-- License:     MIT
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for working with PostgreSQL migrations.

module Database.PostgreSQL.Schema
  (
  -- * Adding Migrations
    add
  -- * Applying Migrations
  , bootstrap
  , converge
  ) where

import BasePrelude hiding ( FilePath, (%), intercalate, lines, forM_, concat, forM, foldr, map, bracket, (++) )
import BasicPrelude hiding ( intercalate, (</>) )
import Data.Text          ( Text, intercalate, lines, strip, unpack )
import Database.PostgreSQL.Simple
import Formatting         ( (%), sformat, stext )
import Shelly


-- experimental

withConn :: Text -> (Connection -> IO a) -> IO a
withConn url =
  bracket (connectPostgreSQL (encodeUtf8 url)) close

query' :: (FromRow f, ToRow t) => Query -> t -> Text -> IO [f]
query' q p url =
  withConn url $ \c ->
    query c q p

countSchema :: Text -> Text -> IO [Only Int]
countSchema schema =
  query' (fromString q) p where
    q = " SELECT count(*) \
        \ FROM pg_namespace \
        \ WHERE nspname = ? "
    p = Only schema

selectMigrations' :: [FilePath] -> Text -> Text -> Text -> IO [Only Text]
selectMigrations' migrations schema table =
  query' (fromString q) p where
    q = " SELECT filename \
        \ FROM " ++ unpack schema ++ "." ++ unpack table ++
        " WHERE filename IN ? "
    p = Only $ In $ map toTextIgnore migrations


-- types

type Migration = (FilePath, FilePath)


-- psql

psqlCommand :: Text -> Text -> Sh Text
psqlCommand c url =
  run "psql" [ "--no-align"
             , "--tuples-only"
             , "--command"
             , c
             , url ]

psqlFile :: FilePath -> Text -> Sh ()
psqlFile f url =
  run_ "psql" [ "--no-align"
              , "--tuples-only"
              , "--quiet"
              , "--file"
              , toTextIgnore f
              , url ]


-- SQL

insertMigration :: FilePath -> Text -> Text -> Text
insertMigration migration table schema =
  sformat ( " INSERT INTO " % stext % "." % stext % " (filename) " %
            " SELECT '" % stext % "' " %
            " WHERE NOT EXISTS " %
            " ( SELECT TRUE FROM " % stext % "." % stext %
            "   WHERE filename = '" % stext % "') " )
    schema table (toTextIgnore migration) schema table (toTextIgnore migration)

selectMigrations :: [FilePath] -> Text -> Text -> Text
selectMigrations migrations table schema =
  sformat ( " SELECT filename " %
            " FROM " % stext % "." % stext %
            " WHERE filename IN ( " % stext % " ) " )
    schema table $
      intercalate ", " $ flip map migrations $ \migration ->
        sformat ("'" % stext % "'") (toTextIgnore migration)


-- psql + SQL

checkSchema :: Text -> Text -> IO Bool
checkSchema schema url = do
  [Only count] <- countSchema schema url
  return $ count == 0

filterMigrations :: [Migration] -> Text -> Text -> Text -> Sh [Migration]
filterMigrations migrations table schema url = do
  r <- psqlCommand (selectMigrations (map snd migrations) table schema) url
  return $ removes ((==) . snd) migrations (map fromText (lines r)) where
    removes p = foldr remove where
      remove x = foldr f [] where
        f a b = if p a x then b else a : b

filterMigrations' migrations table schema url = do
  migrations' <- selectMigrations' migrations schema table url
  return migrations'


-- migrations

ls_f :: FilePath -> Sh [FilePath]
ls_f dir = do
  items <- ls dir
  filterM test_f items

lsMigrations :: FilePath -> Sh [Migration]
lsMigrations dir = do
  migrations <- ls_f dir
  migrations' <- forM migrations $ relativeTo dir
  return $ sortBy (comparing snd) $ zip (repeat dir) migrations'

findMigrations :: FilePath -> Sh [Migration]
findMigrations dir = do
  dirs <- findWhen test_d dir
  migrations <- forM (dir : dirs) lsMigrations
  return $ sortBy (comparing snd) $ concat migrations

searchMigrations :: Bool -> FilePath -> Sh [Migration]
searchMigrations recur =
  if recur then findMigrations else lsMigrations

migrate :: [Migration] -> Text -> Text -> Text -> Sh ()
migrate migrations table schema url =
  forM_ migrations $ uncurry $ \dir migration ->
    chdir dir $ do
      echo $ out migration
      contents <- readfile migration
      withTmpDir $ \dir' ->
        chdir dir' $ do
          appendfile migration "\\set ON_ERROR_STOP true\n\n"
          appendfile migration contents
          appendfile migration $ insertMigration migration table schema
          psqlFile migration url where
            out migration =
              sformat ( "M " % stext % " -> " % stext )
                (toTextIgnore migration) table


-- API

-- | Add a DDL migration file to a migrations directory. Fails if
-- migration file or migrations directory do not exist.
add :: FilePath -> FilePath -> FilePath -> Sh ()
add migration file dir = do
  echo out
  mv file (dir </> migration) where
    out =
      sformat ( "A " % stext % " -> " % stext )
        (toTextIgnore file) (toTextIgnore (dir </> migration))

-- | Apply bootstrap migrations to a database. Checks if a database
-- has been previously bootstrapped, and applies all bootstrap
-- migrations if it has not been previously bootstrapped. Applies
-- all bootstrap migrations that have not been applied yet and records
-- their application.
bootstrap :: FilePath -> Text -> Text -> Text -> Sh ()
bootstrap dir table schema url = do
  migrations <- lsMigrations dir
  check <- liftIO $ checkSchema schema url
  when check $ do
    echo "Bootstrapping..."
    migrate migrations table schema url
  migrations' <- filterMigrations migrations table schema url
  unless (null migrations') $ do
    echo "Bootstrap migrating..."
    migrate migrations' table schema url


-- | Apply migrations to a database. Applies all migrations that have
-- not been applied yet and records their application.
converge :: Bool -> FilePath -> Text -> Text -> Text -> Sh ()
converge recur dir table schema url = do
  migrations <- searchMigrations recur dir
  migrations' <- filterMigrations migrations table schema url
  unless (null migrations') $ do
    echo "Migrating..."
    migrate migrations' table schema url
