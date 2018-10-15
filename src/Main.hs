{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Config
import           Hashing

import           BroadcastChan.Conduit
import           Conduit                  (MonadResource, MonadUnliftIO)
import           Control.Exception.Base   (evaluate)
import           Control.Monad.Reader
import qualified Data.ByteString          as BS
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List        (sourceList)
import           Data.Either              (partitionEithers)
import           Data.List                (findIndex, intercalate)
import           Data.List.NonEmpty       (NonEmpty (..), toList)
import qualified Data.Set.BKTree          as BK
import           System.Directory
import           System.FilePath
import           System.Process.Text      (readProcessWithExitCode)

-- TODO: Copy files to /tmp for comparison with feh

{-
TODO: Support deferring filesystem operations til the end (a la dry run)
import of a single file goes through the states:
- Not imported
- Imported, unmoved
- Imported, moved
-}

type Yielding = (FilePath, ImageInfo)

readFiles :: (MonadReader Config m, MonadResource m, MonadUnliftIO m) => BK.BKTree ImageInfo -> Config -> ConduitT i o m (BK.BKTree ImageInfo, [Yielding])
readFiles images Config { caps, importdir } =
  C.sourceDirectoryDeep True importdir
  .| C.mapM (\path -> (path,) <$> liftIO (BS.readFile path))
  .| parMapM (Simple Drop) caps imageInfoIO
  .| ins images (const yield)
    `fuseBoth`
  C.sinkList
  where
    imageInfoIO (path, bytes) = liftIO $ case getImageInfoBS (takeExtension path) bytes of
      Left err   -> putStrLn err *> fail err
      -- Force evaluation of the hashes on this thread which is run in parallel
      Right info -> (path,) <$> (evaluate . seq (contentHash info) . seq (perceptualHash info) $ info)

ins images action = await >>= \case
  Nothing -> return images
  Just (path, new) -> case search images new of
    Present -> do
      log "Already present, removing the import file"
      liftIO $ removeFile path
      ins images action
    SimilarPictures infos -> do
      log $ show (length infos) ++ " similar image(s) found, running action"
      action infos (path, new)
      ins images action
    NotPresent -> do
      log "New image, importing it"
      doImport (path, new)
      ins (BK.insert new images) action
    where
      log :: MonadIO m => String -> m ()
      log str = liftIO $ putStrLn $ "While testing " ++ path ++ ": " ++ str

main :: IO ()
main = do
  config <- getConfig
  flip runReaderT config $ do
    hashes <- getHashes
    (res, similar) <- runConduitRes (readFiles hashes config)
    runConduit (sourceList similar .| ins res selectiveImport)
    return ()


getHashes :: (MonadIO m, MonadReader Config m) => m (BK.BKTree ImageInfo)
getHashes = do
  hashdir <- asks hashdir
  hashfiles <- liftIO $ fmap (hashdir </>) <$> listDirectory hashdir
  let (errors, images) = partitionEithers $ map getHashImageInfo hashfiles
  if null errors then return $ BK.fromList images
  else error $ intercalate "\n" errors

comparePics :: (MonadReader Config m, MonadIO m) => [FilePath] -> m ()
comparePics pics = do
  feh <- asks feh
  let args = pics ++ ["-A", "rm %F", "-g640x480", "-Z", "-.", "-Bblack"]
  liftIO $ putStrLn $ "Starting feh with arguments " ++ unwords args
  liftIO $ readProcessWithExitCode feh args ""
  return ()

selectiveImport :: (MonadReader Config m, MonadIO m) => NonEmpty ImageInfo -> (FilePath, ImageInfo) -> m ()
selectiveImport candidates (path, new) = do
  paths <- forM (toList candidates) hashbasedFilename
  comparePics (path : paths)
  filethere <- liftIO $ doesFileExist path
  when filethere (doImport (path, new))

doImport :: (MonadReader Config m, MonadIO m) => (FilePath, ImageInfo) -> m ()
doImport (path, new) = do
  importDestination <- hashbasedFilename new
  liftIO $ copyFileWithMetadata path importDestination
  liftIO $ removeFile path


