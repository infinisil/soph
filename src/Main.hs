{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Concurrent.Async.Pool
import           Control.Monad
import           Control.Monad.Reader
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Vector                   as V
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process.Text

mapConcurrentlyBounded :: Traversable t => Int -> (a1 -> IO a) -> t a1 -> IO (t a)
mapConcurrentlyBounded n f args = withTaskGroup n $ flip mapTasks (fmap f args)

data Config = Config
  { files     :: [FilePath]
  , blockhash :: FilePath
  } deriving Show

getConfig :: IO Config
getConfig = do
  dir <- fromMaybe (fail "No argument supplied to use as a directory") . listToMaybe <$> getArgs
  blockhash <- fromMaybe (fail "No hashblock binary found in PATH") <$> findExecutable "blockhash"
  files <- fmap (dir </>) <$> listDirectory dir
  return Config
    { files = files
    , blockhash = blockhash
    }

getHash :: Config -> FilePath -> IO (Maybe (Hash, FilePath))
getHash Config { blockhash } file = do
  (code, stdout, stderr) <- readProcessWithExitCode blockhash [ file ] Text.empty
  case code of
    ExitFailure code -> do
      putStrLn $ "Error code " ++ show code ++ " while getting hash of file " ++ file ++ ": " ++ show stderr ++ show stdout
      return Nothing
    ExitSuccess   -> return $ Just (Hash (V.fromList (Text.unpack (fst (Text.breakOn "  " stdout)))), file)

getHashes :: Config -> IO [(Hash, String)]
getHashes config@Config { files } = catMaybes <$> mapConcurrentlyBounded 8 (getHash config) files

type Results = [(Hash, FilePath)]

newtype Hash = Hash { unHash :: V.Vector Char } deriving Show

hammingDistance :: Hash -> Hash -> Int
hammingDistance (Hash h1) (Hash h2) =
  V.sum $ V.map popCount $ V.zipWith xor (V.map digitToInt h1) (V.map digitToInt h2)

main :: IO ()
main = do
  config <- getConfig
  putStrLn "Calculating Hashes.."
  result <- getHashes config
  doSearch config result

doSearch :: Config -> Results -> IO ()
doSearch config@Config { } results = do
  putStrLn "Enter your search file:"
  cur <- getCurrentDirectory
  file <- (cur </>) <$> getLine
  putStrLn file
  mhash <- getHash config file
  case mhash of
    Just (hash, _) -> do
      putStrLn $ "Hash is " ++ show hash
      case length result of
        0 -> putStrLn "No similar images found"
        _ -> do
          putStrLn "Found similar images:"
          mapM_ print result
        where result = search hash results
    Nothing -> return ()
  doSearch config results

search :: Hash -> Results -> [(Int, FilePath)]
search hash results = top
  where
    y = map (\(h, f) -> (hammingDistance hash h, f)) results
    top = filter (\(d, f) -> d <= 10) . sortBy (comparing fst) $ y

