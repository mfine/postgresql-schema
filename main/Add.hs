-- |
-- Module:      Add
-- Copyright:   (c) 2015 Mark Fine
-- License:     MIT
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Tool for adding PostgreSQL migrations.

import BasicPrelude
import Data.Text ( pack )
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.Schema
import Options.Applicative
import Shelly
import System.Locale

data Args = Args
  { aFile :: String
  , aName :: Maybe String
  , aDir  :: Maybe String
  } deriving ( Eq, Read, Show )

args :: ParserInfo Args
args =
  info ( helper <*> args' )
    (  fullDesc
    <> header   "schema-apply: Apply Schema to PostgreSQL Database"
    <> progDesc "Apply Schema" ) where
    args' = Args
      <$> strOption
          (  long    "file"
          <> metavar "FILE"
          <> help    "Migration File" )
      <*> optional ( strOption
          (  long    "name"
          <> metavar "NAME"
          <> help    "Migration Name" ) )
      <*> optional ( strOption
          (  long    "dir"
          <> metavar "DIR"
          <> help    "Migrations Directory" ) )

exec :: String -> String -> String -> IO ()
exec migration file dir =
  shelly $
    add (fromText (pack migration)) (fromText (pack file)) (fromText (pack dir))

newMigration :: Maybe String -> IO String
newMigration name = do
  now <- getCurrentTime
  return $ intercalate "-" ( timestamp now : maybeToList name ) ++ ".sql" where
    timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S"

main :: IO ()
main =
  execParser args >>= call where
    call Args{..} = do
      migration <- newMigration aName
      exec migration aFile (fromMaybe "migrations" aDir)

