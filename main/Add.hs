-- |
-- Module:      Add
-- Copyright:   (c) 2015 Mark Fine
-- License:     MIT
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Tool for adding PostgreSQL migrations.

import BasePrelude
import Data.Text                  ( pack )
import Data.Time.Clock            ( getCurrentTime )
import Data.Time.Format           ( formatTime )
import Database.PostgreSQL.Schema ( add )
import Options.Applicative
import Shelly
import System.Locale              ( defaultTimeLocale )

data Args = Args
  { aFile :: String
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
          (  long    "dir"
          <> metavar "DIR"
          <> help    "Migrations Directory" ) )

newMigration :: IO String
newMigration = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S.sql" now

exec :: String -> String -> IO ()
exec file dir = do
  migration <- newMigration
  shelly $
    add (fromText (pack migration)) (fromText (pack file)) (fromText (pack dir))

main :: IO ()
main =
  execParser args >>= call where
    call Args{..} =
      exec aFile (fromMaybe "migrations" aDir)

