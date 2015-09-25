-- |
-- Module:      Apply
-- Copyright:   (c) 2015 Mark Fine
-- License:     MIT
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Tool for applying PostgreSQL migrations.

import BasicPrelude hiding ( FilePath )
import Data.Text ( pack )
import Database.PostgreSQL.Schema
import Options.Applicative
import Paths_postgresql_schema
import Shelly
import System.IO hiding ( FilePath )
import System.Environment

data Args = Args
  { aRecur :: Bool
  , aDir   :: Maybe String
  , aUrl   :: Maybe String
  } deriving ( Eq, Read, Show )

args :: ParserInfo Args
args =
  info ( helper <*> args' )
    (  fullDesc
    <> header   "schema-apply: Apply Schema to PostgreSQL Database"
    <> progDesc "Apply Schema" ) where
    args' = Args
      <$> switch
          (  long    "recurse"
          <> help    "Recurse Migrations Directory" )
      <*> optional ( strOption
          (  long    "dir"
          <> metavar "DIR"
          <> help    "Migrations Directory" ) )
      <*> optional ( strOption
          (  long    "url"
          <> metavar "URL"
          <> help    "Database URL" ) )

apply :: Bool -> FilePath -> FilePath -> Text -> Sh ()
apply recur bootstrapDir dir url = do
  bootstrap bootstrapDir bootstrapTable schema url
  converge recur dir table schema url where
    bootstrapTable = "bootstrap_scripts"
    table = "scripts"
    schema = "schema_evolution_manager"

exec :: Bool -> String -> String -> String -> IO ()
exec recur bootstrapDir dir url =
  shelly $
    apply recur (fromText (pack bootstrapDir)) (fromText (pack dir)) (pack url)

main :: IO ()
main =
  execParser args >>= call where
    call Args{..} = do
      url <- lookupEnv "DATABASE_URL"
      bootstrapDir <- getDataFileName "migrations"
      maybe
        (err "No Database URL")
        (exec aRecur bootstrapDir (fromMaybe "migrations" aDir))
        (aUrl <|> url) where
          err = hPutStrLn stderr
