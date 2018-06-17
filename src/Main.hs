{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector          as V
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

data Config = Config
  { files     :: [FilePath]
  , blockhash :: FilePath
  , hashdir   :: FilePath
  , feh       :: FilePath
  } deriving Show

getConfig :: IO Config
getConfig = do
  args <- getArgs
  (hashdir, importdir) <- case args of
    [] -> fail "No first argument supplied, should be hash directory"
    [hashdir] -> fail "No second argument supplied, should be import directory"
    hashdir:importdir:_ -> return (hashdir, importdir)
  files <- fmap (importdir </>) <$> listDirectory importdir
  blockhash <- fromMaybe (fail "No hashblock binary found in PATH") <$> findExecutable "blockhash"
  feh <- fromMaybe (fail "No feh binary found in PATH") <$> findExecutable "feh"
  return Config
    { files = files
    , blockhash = blockhash
    , feh = feh
    , hashdir = hashdir
    }

getHash :: Config -> FilePath -> IO (Maybe Hash)
getHash Config { blockhash } file = do
  (code, stdout, stderr) <- readProcessWithExitCode blockhash [ file ] ""
  case code of
    ExitFailure code -> do
      putStrLn $ "Error code " ++ show code ++ " while getting hash of file " ++ file ++ ": " ++ show stderr ++ show stdout
      return Nothing
    ExitSuccess -> return $ Just . V.fromList . takeWhile (/= ' ') $ stdout

type Hash = V.Vector Char

hammingDistance :: Hash -> Hash -> Int
hammingDistance h1 h2 =
  V.sum $ V.map popCount $ V.zipWith xor (V.map digitToInt h1) (V.map digitToInt h2)

type State = ([Hash], [FilePath])

initial :: Config -> IO State
initial Config { hashdir, files } = do
  hashfiles <- listDirectory hashdir
  return (V.fromList . dropExtensions <$> hashfiles, files)

trans :: Config -> State -> IO (Maybe State)
trans config (hashes, []) = do
  putStrLn "All hashes imported"
  return Nothing
trans config@Config { hashdir, feh } (hashes, file:xs) = do
  mnewhash <- getHash config file
  case mnewhash of
    Nothing -> do
      putStrLn $ "Skipping " ++ file
      return $ Just (hashes, xs)
    Just newhash -> do
      let results = search newhash hashes
      unless (null results) $ do
        putStrLn $ "Delete the ones you don't want of " ++ show results ++ " or the new file " ++ file ++ " (with hash " ++ V.toList newhash ++ ")"
        hashfiles <- mapM (findFileWithHash config) results
        callProcess feh (hashfiles ++ [ file ] ++ ["-A", "rm %F"])
      filethere <- doesFileExist file
      when filethere $ do
        putStrLn $ "Moving file " ++ file ++ " to hashdir with hash " ++ show newhash
        renameFile file (hashdir </> V.toList newhash ++ takeExtensions file)
      return $ Just (newhash : hashes, xs)

findFileWithHash :: Config -> Hash -> IO FilePath
findFileWithHash config@Config{ hashdir } h = do
  all <- listDirectory hashdir
  maybe (fail $ "Couldn't find hash " ++ chars) return . listToMaybe . map (hashdir </>) . filter ((== chars) .  dropExtensions) $ all
  where chars = V.toList h

main :: IO ()
main = do
  config <- getConfig
  putStrLn "initializing"
  state <- initial config
  loop config state

loop :: Config -> State -> IO ()
loop config state = do
  mnextstate <- trans config state
  case mnextstate of
    Nothing        -> return ()
    Just nextstate -> loop config nextstate

search :: Hash -> [Hash] -> [Hash]
search hash = map snd . filter ((10 >=) . fst) . sortBy (comparing fst) . map (\h -> (hammingDistance hash h, h))

