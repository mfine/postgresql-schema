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
  -- * Clearing Migrations
  , clear
  ) where

import BasicPrelude hiding ( FilePath, (</>) )
import Data.Text ( unpack )
import Database.PostgreSQL.Simple
import Shelly

-- types

type Migration = (FilePath, FilePath)

-- SQL

countSchemaSQL :: Text
countSchemaSQL =
  " SELECT count(*) \
  \ FROM pg_namespace \
  \ WHERE nspname = ? "

selectMigrationsSQL :: Text -> Text -> Text
selectMigrationsSQL table schema =
  " SELECT filename \
  \ FROM " <> schema <> "." <> table <>
  " WHERE filename IN ? "

insertMigrationSQL :: FilePath -> Text -> Text -> Text
insertMigrationSQL migration table schema =
  " INSERT INTO " <> schema <> "." <> table <> " (filename) \
  \ SELECT '" <> toTextIgnore migration <> "' \
  \ WHERE NOT EXISTS \
  \   ( SELECT TRUE FROM " <> schema <> "." <> table <>
  "     WHERE filename = '" <> toTextIgnore migration <> "' ) "

dropSchemaSQL :: Text -> Text
dropSchemaSQL schema =
  " DROP SCHEMA IF EXISTS " <> schema <> " CASCADE "

createSchemaSQL :: Text -> Text
createSchemaSQL schema =
  " CREATE SCHEMA IF NOT EXISTS " <> schema <> " "

-- psql

psql :: FilePath -> Text -> Sh ()
psql migration url =
  run_ "psql" [ "--no-align"
              , "--tuples-only"
              , "--quiet"
              , "--single-transaction"
              , "--file"
              , toTextIgnore migration
              , url ]

-- queries

query' :: (FromRow f, ToRow t) => Text -> t -> Text -> IO [f]
query' q p url =
  bracket (connectPostgreSQL (encodeUtf8 url)) close $ \c ->
    query c (fromString $ unpack q) p

execute_' :: Text -> Text -> IO ()
execute_' q url =
  bracket (connectPostgreSQL (encodeUtf8 url)) close $ \c ->
    void $ execute_ c (fromString $ unpack q)

countSchema :: Text -> Text -> IO [Only Int]
countSchema schema =
  query' countSchemaSQL $ Only schema

selectMigrations :: [FilePath] -> Text -> Text -> Text -> IO [Only Text]
selectMigrations migrations table schema =
  query' (selectMigrationsSQL table schema) $ Only $ In $ map toTextIgnore migrations

dropSchema :: Text -> Text -> IO ()
dropSchema schema =
  execute_' (dropSchemaSQL schema)

createSchema :: Text -> Text -> IO ()
createSchema schema =
  execute_' (createSchemaSQL schema)

-- interpreted queries

checkSchema :: Text -> Text -> IO Bool
checkSchema schema url = do
  result <- countSchema schema url
  return $ maybe False ((== 0) . fromOnly) (listToMaybe result)

filterMigrations :: [Migration] -> Text -> Text -> Text -> IO [Migration]
filterMigrations migrations table schema url = do
  results <- selectMigrations (map snd migrations) table schema url
  return $ removes ((==) . snd) migrations (map (fromText . fromOnly) results) where
    removes p = foldr remove where
      remove x = foldr f [] where
        f a b = if p a x then b else a : b

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
      echo $ "M " <> toTextIgnore migration <> " -> " <> table
      contents <- readfile migration
      withTmpDir $ \dir' ->
        chdir dir' $ do
          appendfile migration "\\set ON_ERROR_STOP true\n\n"
          appendfile migration contents
          appendfile migration $ insertMigrationSQL migration table schema
          psql migration url

-- API

-- | Add a DDL migration file to a migrations directory. Fails if
-- migration file or migrations directory do not exist.
add :: FilePath -> FilePath -> FilePath -> Sh ()
add migration file dir = do
  echo $ "A " <> toTextIgnore file <> " -> " <> toTextIgnore (dir </> migration)
  mv file (dir </> migration)

-- | Apply bootstrap migrations to a database. Checks if a database
-- has been previously bootstrapped, and applies all bootstrap
-- migrations if it has not been previously bootstrapped. Applies
-- all bootstrap migrations that have not been applied yet and records
-- their application.
bootstrap :: FilePath -> Text -> Text -> Text -> Sh ()
bootstrap dir table schema url = do
  migrations <- lsMigrations dir
  check <- liftIO $ checkSchema schema url
  if check then do
    echo "Bootstrapping..."
    migrate migrations table schema url
  else do
    migrations' <- liftIO $ filterMigrations migrations table schema url
    unless (null migrations') $ do
      echo "Bootstrap migrating..."
      migrate migrations' table schema url

-- | Apply migrations to a database. Applies all migrations that have
-- not been applied yet and records their application.
converge :: Bool -> FilePath -> Text -> Text -> Text -> Sh ()
converge recur dir table schema url = do
  migrations <- searchMigrations recur dir
  migrations' <- liftIO $ filterMigrations migrations table schema url
  unless (null migrations') $ do
    echo "Migrating..."
    migrate migrations' table schema url

clear :: Text -> Text -> Sh ()
clear schema url = do
  echo "Dropping..."
  liftIO $ do
    dropSchema schema url
    dropSchema "public" url
    createSchema "public" url
