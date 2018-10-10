{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Picture            as P
import           Control.Applicative      (liftA2)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Blockhash
import           Data.ByteArray.Hash      (FnvHash64 (..), fnv1a_64Hash)
import qualified Data.ByteString          as BS
import           Data.Char                (isHexDigit)
import           Data.Either              (partitionEithers)
import           Data.List                (findIndex, intercalate)
import           Data.List.NonEmpty       (NonEmpty (..), toList)
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector.Unboxed      as V
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process.Text
import           Text.Printf

import           Control.Concurrent       (ThreadId, forkIO, getNumCapabilities,
                                           threadDelay)
import           Control.Concurrent.Async (replicateConcurrently_)
import           Control.Concurrent.STM
import           Control.Monad

import           Config
import           Hashing

{-
TODO:
- Multiple imports at the same time using STM
- Copy files to /tmp for comparison with feh
-}

main :: IO ()
main = do
  config <- getConfig
  runStdoutLoggingT $ flip runReaderT config $ do
    hashes <- getHashes
    hashesVar <- liftIO $ newTVarIO hashes
    importdir <- asks importdir
    queue <- initWorkQueue
    similarQueue <- liftIO newTQueueIO
    spawnWorkers hashesVar queue similarQueue
    liftIO $ putStrLn "Done"
    handleSimilars hashesVar similarQueue
    --evalStateT (importFiles toimport) hashes
    return ()

handleSimilars :: (MonadReader Config m, MonadIO m) => Images -> TQueue ImageInfo -> m ()
handleSimilars images queue = do
  items <- liftIO $ atomically $ allQueueItems queue
  config <- ask
  forM_ items $ \item -> do
    dry <- asks dryRun
    importDestination <- if dry then return (path item) else hashbasedFilename item
    importSingle (\infos -> do
        atomically $ modifyTVar images (item { path = importDestination }:)
        runReaderT (selectiveImport infos item) config)
      images item
    return ()

allQueueItems :: TQueue a -> STM [a]
allQueueItems queue = do
  value <- tryReadTQueue queue
  case value of
    Nothing   -> return []
    Just item -> (item:) <$> allQueueItems queue

type WorkItem = FilePath
type WorkQueue = TQueue WorkItem

type SimilarQueue = TQueue ImageInfo
type Images = TVar [ImageInfo]

initWorkQueue :: (MonadIO m, MonadReader Config m) => m WorkQueue
initWorkQueue = do
  importdir <- asks importdir
  toimport <- liftIO $ fmap (importdir </>) <$> listDirectory importdir
  liftIO $ atomically $ do
    queue <- newTQueue
    forM_ toimport (writeTQueue queue)
    return queue

spawnWorkers :: (MonadReader Config m, MonadIO m) => Images -> WorkQueue -> SimilarQueue -> m ()
spawnWorkers images queue similar = do
  caps <- liftIO getNumCapabilities
  liftIO $ putStrLn $ "Starting " ++ show caps ++ " threads"
  config <- ask
  liftIO $ replicateConcurrently_ caps (runReaderT (worker images queue similar ) config)

worker :: (MonadIO m, MonadReader Config m) => Images -> WorkQueue -> SimilarQueue -> m ()
worker images queue similar = do
  mitem <- liftIO $ atomically $ tryReadTQueue queue
  case mitem of
    Nothing -> return ()
    Just item -> do
      liftIO $ putStrLn $ "Handling item " ++ item
      minfo <- getImageInfo item
      case minfo of
        Left error -> liftIO $ putStrLn error
        Right info -> importSingle (\_ -> atomically $ writeTQueue similar info) images info
      worker images queue similar


getHashes :: (MonadIO m, MonadReader Config m) => m [ImageInfo]
getHashes = do
  hashdir <- asks hashdir
  hashfiles <- liftIO $ fmap (hashdir </>) <$> listDirectory hashdir
  let (errors, images) = partitionEithers $ map getHashImageInfo hashfiles
  if null errors then return images
  else error $ intercalate "\n" errors

comparePics :: (MonadReader Config m, MonadIO m) => [FilePath] -> m ()
comparePics pics = do
  feh <- asks feh
  let args = pics ++ ["-A", "rm %F"]
  dry <- asks dryRun
  liftIO $ putStrLn $ "Would start feh with arguments " ++ unwords args
  liftIO $ if dry then do
    putStrLn $ "Would start feh with arguments " ++ unwords args
    return ()
  else do
    readProcessWithExitCode feh args ""
    return ()

selectiveImport :: (MonadReader Config m, MonadIO m) => NonEmpty ImageInfo -> ImageInfo -> m ()
selectiveImport candidates new = do
  comparePics (newpath : map path (toList candidates))
  filethere <- liftIO $ doesFileExist newpath
  when filethere (doImport new)
  where newpath = path new

doImport :: (MonadReader Config m, MonadIO m) => ImageInfo -> m ()
doImport new = do
  importDestination <- hashbasedFilename new
  dry <- asks dryRun
  newItem <- if dry then do
    liftIO $ putStrLn ("Would copy file " ++ path new ++ " to " ++ importDestination)
    return new
  else do
    liftIO $ copyFileWithMetadata (path new) importDestination
    return new { path = importDestination }
  unless dry $ liftIO $ removeFile (path new)


importSingle :: (MonadIO m, MonadReader Config m) => (NonEmpty ImageInfo -> IO ()) -> Images -> ImageInfo -> m ()
importSingle simhandle imagesv new = do
  action <- liftIO $ atomically $ do
    images <- readTVar imagesv
    case search images new of
      Present -> return $ do
        liftIO $ putStrLn $ "Image already present"
        liftIO $ removeFile (path new)
      SimilarPictures infos -> return $ do
        liftIO $ putStrLn $ "Found similar pictures: " ++ show infos
        liftIO $ simhandle infos
      NotPresent -> do
        writeTVar imagesv (new:images)
        return $ do
          liftIO $ putStrLn "Image not present"
          doImport new
  action

-- TODO: Replace with BKTree (bktrees package)

search :: [ImageInfo] -> ImageInfo -> SearchResult
search images new = mconcat $ liftA2 ($) toResult (compareImages 12 new) <$> images
  where toResult _ Same      = Present
        toResult img Similar = SimilarPictures (img :| [])
        toResult _ Different = NotPresent

data SearchResult = NotPresent
                  | SimilarPictures (NonEmpty ImageInfo)
                  | Present

instance Semigroup SearchResult where
  Present <> _ = Present
  _ <> Present = Present
  NotPresent <> NotPresent = NotPresent
  NotPresent <> SimilarPictures ys = SimilarPictures ys
  SimilarPictures xs <> NotPresent = SimilarPictures xs
  SimilarPictures xs <> SimilarPictures ys = SimilarPictures (xs <> ys)

instance Monoid SearchResult where
  mempty = NotPresent

data ImageDifference = Same -- Same content hash
                     | Similar -- Similar or same perceptual hash, different content hash
                     | Different -- Very different perceptual hash, different content hash
                     deriving (Ord, Eq)

compareImages :: Int -> ImageInfo -> ImageInfo -> ImageDifference
compareImages similarDist left right
  | contentHash left == contentHash right = Same
  | perceptualHash left `hammingDistance` perceptualHash right < similarDist = Similar
  | otherwise = Different
