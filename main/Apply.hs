import BasePrelude         hiding ( FilePath )
import Data.Text                  ( Text, pack )
import Database.PostgreSQL.Schema ( bootstrap, converge )
import Options.Applicative
import Paths_postgresql_schema    ( getDataFileName )
import Shelly

data Args = Args
  { aDir :: Maybe String
  , aUrl :: Maybe String
  } deriving ( Eq, Read, Show )

args :: ParserInfo Args
args =
  info ( helper <*> args' )
    (  fullDesc
    <> header   "schema-apply: Apply Schema to PostgreSQL Database"
    <> progDesc "Apply Schema" ) where
    args' = Args
      <$> optional ( strOption
          (  long    "dir"
          <> metavar "DIR"
          <> help    "Migrations Directory" ) )
      <*> optional ( strOption
          (  long    "url"
          <> metavar "URL"
          <> help    "Database URL" ) )

apply :: FilePath -> FilePath -> Text -> Sh ()
apply bootstrapDir dir url = do
  bootstrap bootstrapDir bootstrapTable schema url
  converge dir table schema url where
    bootstrapTable = "bootstrap_scripts"
    table = "scripts"
    schema = "schema_evolution_manager"

exec :: String -> String -> String -> IO ()
exec bootstrapDir dir url =
  shelly $
    apply (fromText (pack bootstrapDir)) (fromText (pack dir)) (pack url)

main :: IO ()
main =
  execParser args >>= call where
    call Args{..} = do
      url <- lookupEnv "DATABASE_URL"
      bootstrapDir <- getDataFileName "migrations"
      maybe (return ()) (exec bootstrapDir (fromMaybe "migrations" aDir)) (aUrl <|> url)
