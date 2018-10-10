module Config where

import           Control.Concurrent (getNumCapabilities)
import           Control.Monad      (when)
import           Data.Maybe         (fromMaybe)
import           System.Directory   (findExecutable)
import           System.Environment (getArgs)

data Config = Config
  { importdir :: FilePath
  , hashdir   :: FilePath
  , feh       :: FilePath
  , dryRun    :: Bool
  , caps      :: Int
  } deriving Show

getConfig :: IO Config
getConfig = do
  args <- getArgs
  (hashdir, importdir, dry) <- case args of
    [] -> fail "No first argument supplied, should be hash directory"
    [hashdir] -> fail "No second argument supplied, should be import directory"
    hashdir:importdir:"-n":_ -> return (hashdir, importdir, True)
    hashdir:importdir:_ -> return (hashdir, importdir, False)
  when dry $ putStrLn "Dry run"
  feh <- fromMaybe (fail "No feh binary found in PATH") <$> findExecutable "feh"
  caps <- getNumCapabilities
  return Config
    { importdir = importdir
    , feh = feh
    , hashdir = hashdir
    , dryRun = dry
    , caps = caps
    }
