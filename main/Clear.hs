-- |
-- Module:      Clear
-- Copyright:   (c) 2015 Mark Fine
-- License:     MIT
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Tool for clearing PostgreSQL migrations.

import BasicPrelude
import Data.Text                  (pack)
import Database.PostgreSQL.Schema
import Options.Applicative
import Shelly
import System.Environment
import System.IO                  hiding (getLine, putStr)

data Args = Args
  { aUrl :: Maybe String
  , aYes :: Bool
  } deriving ( Eq, Read, Show )

args :: ParserInfo Args
args =
  info ( helper <*> args' )
    (  fullDesc
    <> header   "schema-apply: Apply Schema to PostgreSQL Database"
    <> progDesc "Apply Schema" ) where
    args' = Args
      <$> optional ( strOption
          (  long    "url"
          <> metavar "URL"
          <> help    "Database URL" ) )
      <*> switch
          (  long "yes"
          <> help  "Assume 'yes'" )

prompt :: String -> IO Bool
prompt url = do
  putStr $ "Really drop " <> pack url <> "? [y]: "
  hFlush stdout
  (== "y") <$> getLine

exec :: Bool -> String -> IO ()
exec yes url = do
  y <- if yes then pure True else prompt url
  when y $ shelly $
    clear schema (pack url) where
      schema = "schema_evolution_manager"

main :: IO ()
main =
  execParser args >>= call where
    call Args{..} = do
      url <- lookupEnv "DATABASE_URL"
      maybe (err "No Database URL") (exec aYes) (aUrl <|> url) where
        err = hPutStrLn stderr
