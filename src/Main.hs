{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as Text
import qualified Data.Vector          as V
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process.Text

data Config = Config
  { importdir :: FilePath
  , hashdir   :: FilePath
  , blockhash :: FilePath
  , feh       :: FilePath
  } deriving Show

getConfig :: IO Config
getConfig = do
  args <- getArgs
  (hashdir, importdir) <- case args of
    [] -> fail "No first argument supplied, should be hash directory"
    [hashdir] -> fail "No second argument supplied, should be import directory"
    hashdir:importdir:_ -> return (hashdir, importdir)
  blockhash <- fromMaybe (fail "No hashblock binary found in PATH") <$> findExecutable "blockhash"
  feh <- fromMaybe (fail "No feh binary found in PATH") <$> findExecutable "feh"
  return Config
    { importdir = importdir
    , blockhash = blockhash
    , feh = feh
    , hashdir = hashdir
    }

showT :: Show a => a -> Text
showT = pack . show

getHash :: (MonadLogger m, MonadIO m, MonadReader Config m) => FilePath -> m (Maybe Hash)
getHash file = do
  blockhash <- asks blockhash
  (code, stdout, stderr) <- liftIO $ readProcessWithExitCode blockhash [ file ] ""
  case code of
    ExitFailure code -> do
      logErrorN $ "Error code " <> showT code <> " while getting hash of file " <> pack file <> ": " <> stderr <> stdout
      return Nothing
    ExitSuccess -> return $ Just . V.fromList . unpack . fst . Text.breakOn " " $ stdout

type Hash = V.Vector Char

hammingDistance :: Hash -> Hash -> Int
hammingDistance h1 h2 =
  V.sum $ V.map popCount $ V.zipWith xor (V.map digitToInt h1) (V.map digitToInt h2)

data ImportState = ImportState
  { hashes  :: [Hash]
  , skipped :: [FilePath]
  }

getHashes :: (MonadIO m, MonadReader Config m) => m [Hash]
getHashes = do
  hashdir <- asks hashdir
  hashfiles <- liftIO $ listDirectory hashdir
  return $ V.fromList . dropExtensions <$> hashfiles

comparePics :: (MonadReader Config m, MonadIO m) => [FilePath] -> m ()
comparePics pics = do
  feh <- asks feh
  liftIO $ readProcessWithExitCode feh (pics ++ ["-A", "rm %F"]) ""
  return ()

trans :: (MonadLogger m, MonadIO m, MonadState ImportState m, MonadReader Config m) => FilePath -> m ()
trans file = do
  mnewhash <- getHash file
  case mnewhash of
    Nothing -> do
      logInfoN $ "Skipping " <> pack file
      modify (\state@ImportState { skipped } -> state { skipped = takeFileName file : skipped })
    Just newhash -> do
      results <- search newhash
      unless (null results) $ if newhash `notElem` results
        then do
          logInfoN $ "Delete the ones you don't want of " <> Text.concat (map (pack . show) results) <> " or the new file " <> pack file <> " (with hash " <> pack (V.toList newhash) <> ")"
          hashfiles <- mapM findFileWithHash results
          comparePics $ file : hashfiles
        else do
          logInfoN $ "Removing file " <> pack file <> " as it's already present with hash " <> showT newhash
          liftIO $ removeFile file
      filethere <- liftIO $ doesFileExist file
      when filethere $ do
        logInfoN $ "Moving file " <> pack file <> " to hashdir with hash " <> pack (show newhash)
        hashdir <- asks hashdir
        let target = hashdir </> V.toList newhash ++ takeExtension file
        liftIO $ catch (renameFile file target) (\e -> do
                                                    putStrLn $ show (e :: SomeException) ++ "\ncopying instead.."
                                                    copyFileWithMetadata file target
                                                    removeFile file
                                                    )
      hashes <- getHashes
      modify (\state -> state { hashes = hashes })

hand :: SomeException -> IO ()
hand e = return ()

findFileWithHash :: (MonadIO m, MonadReader Config m) => Hash -> m FilePath
findFileWithHash h = do
  hashdir <- asks hashdir
  all <- liftIO $ listDirectory hashdir
  maybe (fail $ "Couldn't find hash " ++ chars) return . listToMaybe . map (hashdir </>) . filter ((== chars) .  dropExtensions) $ all
  where chars = V.toList h

importFiles :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadState ImportState m) => m ()
importFiles = do
  importdir <- asks importdir
  skipped <- gets skipped
  files <- liftIO $ fmap (importdir </>) . listToMaybe . filter (`notElem` skipped) <$> listDirectory importdir
  maybe (logInfoN "Imported all files") ((*> importFiles) . trans) files


main :: IO ()
main = do
  config <- getConfig
  runStdoutLoggingT $ flip runReaderT config $ do
    hashes <- getHashes
    evalStateT importFiles (ImportState hashes [])

search :: MonadState ImportState m => Hash -> m [Hash]
search hash = gets $ map snd . filter ((10 >=) . fst) . sortBy (comparing fst) . map (\h -> (hammingDistance hash h, h)) . hashes

