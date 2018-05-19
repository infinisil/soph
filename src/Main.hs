module Main where

import System.Process.Text
import qualified Data.Text as Text
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath
import Control.Concurrent.Async.Pool
import System.Exit
import Data.Maybe
import qualified Data.Vector as V
import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import System.Environment

blockhash = "/home/infinisil/src/nixpkgs/result/bin/blockhash"

mapConcurrentlyBounded :: Traversable t => Int -> (a1 -> IO a) -> t a1 -> IO (t a)
mapConcurrentlyBounded n f args = withTaskGroup n $ flip mapTasks (fmap f args)

getHashes :: IO [(Text, String)]
getHashes = do
  dir <- head <$> getArgs
  files <- listDirectory dir
  let absolute = map (dir </>) files
  catMaybes <$> mapConcurrentlyBounded 8 getHash absolute
    where
      getHash :: FilePath -> IO (Maybe (Text, FilePath))
      getHash file = do
        (code, stdout, _) <- readProcessWithExitCode blockhash [ file ] Text.empty
        case code of
          ExitFailure _ -> return Nothing
          ExitSuccess -> return $ Just (stdout, file)

mapResults :: (Text, FilePath) -> (Hash, FilePath)
mapResults (text, fp) = (Hash (V.fromList (Text.unpack text)), fp)

type Results = [(Hash, FilePath)]

newtype Hash = Hash { unHash :: V.Vector Char } deriving Show

hammingDistance :: Hash -> Hash -> Int
hammingDistance (Hash h1) (Hash h2) =
  V.sum $ V.map popCount $ V.zipWith xor (V.map digitToInt h1) (V.map digitToInt h2)

main :: IO ()
main = do
  putStrLn "Calculating Hashes.."
  result <- fmap mapResults <$> getHashes
  doSearch result

doSearch :: Results -> IO ()
doSearch results = do
  putStrLn "Enter your search hash: "
  input <- getLine
  let result = search input results
  putStrLn "Found similar hashes: "
  mapM_ print result
  doSearch results

search :: String -> Results -> [(Int, FilePath)]
search hash results = top
  where
    newHash = Hash (V.fromList hash)
    y = map (\(h, f) -> (hammingDistance newHash h, f)) results
    top = filter (\(d, f) -> d <= 10) . sortBy (comparing fst) $ y
  
