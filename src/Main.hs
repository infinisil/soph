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
import qualified Data.Text                as Text
import           System.Directory
import           System.FilePath
import           System.Process.Text      (readProcessWithExitCode)

import           Control.Concurrent       (myThreadId)
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Data.Functor             (($>))

import           Log

-- TODO: Copy files to /tmp for comparison with feh

{-
TODO: Support deferring filesystem operations til the end (a la dry run)
import of a single file goes through the states:
- Not imported
- Imported, unmoved
- Imported, moved
-}

type Yielding = (FilePath, ImageInfo)

readFiles :: (MonadLogger m, MonadReader Config m, MonadResource m, MonadUnliftIO m) => BK.BKTree ImageInfo -> Config -> ConduitT i o m (BK.BKTree ImageInfo, [Yielding])
readFiles images Config { caps, options } = do
  files <- runConduit $ C.sourceDirectoryDeep True (importdir options) .| C.sinkList
  logInfoNS "process" $ "Starting image processing of " <> Text.pack (show (length files)) <> " files in import directory"
  todo <- liftIO $ newTVarIO (length files)
  (sourceList files
    .| C.mapM (\path -> do
                  bytes <- liftIO $ BS.readFile path
                  logDebugNS "process" ("Successfully read bytes from file" <> Text.pack path)
                  return (path, bytes)
              )
    .| parMapM (Simple Drop) caps (imageInfoIO todo)
    .| importer images (\info y -> yield y $> False)
    ) `fuseBoth` C.sinkList
  where
    imageInfoIO todo (path, bytes) = do
      tid <- liftIO $ Text.pack . show <$> myThreadId
      index <- show <$> liftIO (readTVarIO todo)
      logDebugNS ("process-" <> tid) $ "[" <> Text.pack index <> "] Processing bytes from path " <> Text.pack path
      result <- case getImageInfoBS (takeExtension path) bytes of
        Left err   -> do
          logErrorNS ("process-" <> tid) ("Error while decoding image info from path " <> Text.pack path <> ": " <> Text.pack err)
          liftIO $ fail err
        -- Force evaluation of the hashes on this thread which is run in parallel
        Right info -> do
          logDebugNS ("process-" <> tid) ("Successfully processed bytes from path " <> Text.pack path)
          liftIO $ (path,) <$> (evaluate . seq (contentHash info) . seq (perceptualHash info) $ info)
      liftIO $ atomically $ modifyTVar' todo (\x -> x - 1)
      return result



importer :: (MonadIO m, MonadReader Config m, MonadLogger m) => BK.BKTree ImageInfo -> (NonEmpty ImageInfo -> Yielding -> ConduitT Yielding o m Bool) -> ConduitT Yielding o m (BK.BKTree ImageInfo)
importer images action = await >>= \case
  Nothing -> do
    logDebugNS "process" "Done processing new images"
    return images
  Just (path, new) -> case search images new of
    Present -> do
      presentFile <- hashbasedFilename new
      logAction $ "Already present as " <> Text.pack presentFile <> ", removing the import file"
      liftIO $ removeFile path
      importer images action
    SimilarPictures infos -> do
      logAction $ Text.pack (show (length infos)) <> " similar image(s) found"
      importit <- action infos (path, new)
      -- FIXME: when action does importing, it should insert the new one into the tree
      importer (if importit then BK.insert new images else images) action
    NotPresent -> do
      logAction "New image, importing it"
      doImport (path, new)
      importer (BK.insert new images) action
    where logAction str = logInfoNS "process" ("New image at " <> Text.pack path <> ": " <> str)



main :: IO ()
main = do
  config <- getConfig
  withLogs (logFilter config) $ \queue -> runQueueLoggingT queue $ flip runReaderT config $
    getHashes >>= \case
      Nothing -> return ()
      Just hashes -> do
        (res, similar) <- runConduitRes (readFiles hashes config)
        logInfoNS "similars" $ "Processing " <> Text.pack (show (length similar)) <> " similar images"
        final <- runConduit (sourceList similar .| importer res selectiveImport)
        logInfoN $ "Finished import of " <> Text.pack (show (BK.size final - BK.size hashes)) <> " images"


getHashes :: (MonadLogger m, MonadIO m, MonadReader Config m) => m (Maybe (BK.BKTree ImageInfo))
getHashes = do
  logInfoNS "init" "Reading hashdir, decoding filenames and initializing database"
  hashdir <- asks $ hashdir . options
  hashfiles <- liftIO $ fmap (hashdir </>) <$> listDirectory hashdir
  logDebugNS "init" ("Decoding " <> textLength hashfiles <> " hashdir filenames")
  let (errors, images) = partitionEithers $ map getHashImageInfo hashfiles
  if null errors then do
    logDebugNS "init" "Successfully read hashdir"
    return $ Just $ BK.fromList images
  else do
    logErrorNS "init" ("Failed reading " <> textLength errors <> " filenames:")
    forM_ errors $ logErrorNS "init" . ("  "<>) . Text.pack
    return Nothing
  where
    textLength :: [a] -> Text.Text
    textLength = Text.pack . show . length

comparePics :: (MonadLogger m, MonadReader Config m, MonadIO m) => [FilePath] -> m ()
comparePics pics = do
  feh <- asks feh
  let args = pics ++ ["-A", "rm %F", "-g640x480", "-Z", "-.", "-Bblack"]
  logDebugNS "similars" ("Starting feh with arguments " <> Text.pack (show args))
  liftIO $ readProcessWithExitCode feh args ""
  return ()

selectiveImport :: (MonadLogger m, MonadReader Config m, MonadIO m) => NonEmpty ImageInfo -> (FilePath, ImageInfo) -> m Bool
selectiveImport candidates (path, new) = do
  paths <- forM (toList candidates) hashbasedFilename
  logInfoNS "similar" $ "File " <> Text.pack path <> " to import has " <> Text.pack (show (length paths)) <> " similar images: "
  forM_ paths $ logInfoNS "similar" . ("  " <>) . Text.pack
  logInfoNS "similar" "  Opening them and the one to import in feh, delete the ones you don't want with <Enter>, then quit feh with <q>"
  comparePics (path : paths)
  filethere <- liftIO $ doesFileExist path
  if filethere then do
    logDebugNS "similar" $ "Path " <> Text.pack path <> " was not deleted, importing it"
    doImport (path, new)
    return True
  else do
    logDebugNS "similar" $ "Path " <> Text.pack path <> " was deleted, not importing"
    return False

doImport :: (MonadLogger m, MonadReader Config m, MonadIO m) => (FilePath, ImageInfo) -> m ()
doImport (path, new) = do
  importDestination <- hashbasedFilename new
  logInfoNS "process" $ "Importing new file " <> Text.pack path <> " into library to path " <> Text.pack importDestination
  liftIO $ copyFileWithMetadata path importDestination
  liftIO $ removeFile path


