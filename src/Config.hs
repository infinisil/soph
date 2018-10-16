module Config where

import           Data.Maybe           (fromMaybe)
import           System.Directory     (findExecutable)

import           Control.Concurrent   (getNumCapabilities)
import           Control.Monad.Logger
import           Options.Applicative

data Config = Config
  { options   :: Options
  , feh       :: FilePath
  , caps      :: Int
  , logFilter :: LogLevel -> Bool
  }

data Options = Options
  { importdir :: FilePath
  , hashdir   :: FilePath
  , verbose   :: Bool
  }

parser :: Parser Options
parser = Options
  <$> argument str
      ( metavar "SOURCE"
     <> help "Directory to import from" )
  <*> argument str
      ( metavar "TARGET"
     <> help "Directory to import to" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output"
      )

opts :: ParserInfo Options
opts = info (parser <**> helper)
   ( fullDesc
  <> progDesc "Import files"
  <> header "hashsearch - import directories"
   )



getConfig :: IO Config
getConfig = do
  options <- execParser opts
  feh <- fromMaybe (fail "No feh binary found in PATH") <$> findExecutable "feh"
  caps <- getNumCapabilities
  return $ Config
    { feh = feh
    , options = options
    , caps = caps
    , logFilter = if verbose options then const True
        else (>= LevelInfo)
    }
