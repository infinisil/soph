{-# LANGUAGE NamedFieldPuns #-}

module Config where

import           Control.Concurrent  (getNumCapabilities)
import           Control.Monad       (when)
import           Data.Maybe          (fromMaybe)
import           System.Directory    (findExecutable)
import           System.Environment  (getArgs)

import           Options.Applicative

data Config = Config
  { importdir :: FilePath
  , hashdir   :: FilePath
  , feh       :: FilePath
  , caps      :: Int
  } deriving Show

data Options = Options
  { _importdir :: FilePath
  , _hashdir   :: FilePath
  , _verbose   :: Bool
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
  Options { _importdir, _hashdir } <- execParser opts
  feh <- fromMaybe (fail "No feh binary found in PATH") <$> findExecutable "feh"
  caps <- getNumCapabilities
  return Config
    { importdir = _importdir
    , feh = feh
    , hashdir = _hashdir
    , caps = caps
    }
