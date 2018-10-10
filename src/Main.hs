{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Picture        as P
import           Control.Applicative  (liftA2)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Blockhash
import           Data.ByteArray.Hash  (FnvHash64 (..), fnv1a_64Hash)
import qualified Data.ByteString      as BS
import           Data.Char            (isHexDigit)
import           Data.Either          (partitionEithers)
import           Data.List            (findIndex, intercalate)
import           Data.List.NonEmpty   (NonEmpty (..), toList)
import           Data.Maybe           (fromMaybe)
import qualified Data.Vector.Unboxed  as V
import           Numeric              (readHex)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process.Text
import           Text.Printf

{-
TODO:
- Multiple imports at the same time using STM
- Copy files to /tmp for comparison with feh
-}

instance Eq Hash where
  (Hash l) == (Hash r) = l == r


main :: IO ()
main = do
  config <- getConfig
  runStdoutLoggingT $ flip runReaderT config $ do
    hashes <- getHashes
    importdir <- asks importdir
    toimport <- liftIO $ fmap (importdir </>) <$> listDirectory importdir
    evalStateT (importFiles toimport) hashes

data Config = Config
  { importdir :: FilePath
  , hashdir   :: FilePath
  , feh       :: FilePath
  , dryRun    :: Bool
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
  return Config
    { importdir = importdir
    , feh = feh
    , hashdir = hashdir
    , dryRun = dry
    }

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
  liftIO $ if dry then
    putStrLn $ "Would start feh with arguments " ++ unwords args
  else do
    readProcessWithExitCode feh args ""
    return ()

selectiveImport :: (MonadReader Config m, MonadIO m, MonadState [ImageInfo] m) => NonEmpty ImageInfo -> ImageInfo -> m ()
selectiveImport candidates new = do
  comparePics (newpath : map path (toList candidates))
  filethere <- liftIO $ doesFileExist newpath
  when filethere (doImport new)
  where newpath = path new

doImport :: (MonadReader Config m, MonadIO m, MonadState [ImageInfo] m) => ImageInfo -> m ()
doImport new = do
  importDestination <- hashbasedFilename new
  dry <- asks dryRun
  liftIO $ if dry then putStrLn ("Would copy file " ++ path new ++ " to " ++ importDestination) else
    copyFileWithMetadata (path new) importDestination
  unless dry $ liftIO $ removeFile (path new)
  modify (new { path = importDestination } :)


importSingle :: (MonadLogger m, MonadIO m, MonadState [ImageInfo] m, MonadReader Config m) => ImageInfo -> m ()
importSingle new = do
  images <- get
  case search images new of
    Present           -> do
      liftIO $ putStrLn $ "Image already present"
      liftIO $ removeFile (path new)
    SimilarPictures infos -> do
      liftIO $ putStrLn $ "Found similar pictures: " ++ show infos
      selectiveImport infos new
    NotPresent        -> do
      liftIO $ putStrLn "Image not present"
      doImport new

importFiles :: (MonadLogger m, MonadIO m, MonadState [ImageInfo] m, MonadReader Config m) => [FilePath] -> m ()
importFiles ps = forM_ ps $ \p -> do
  minfo <- getImageInfo p
  case minfo of
    Left error -> liftIO $ putStrLn error
    Right info -> importSingle info


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

blockhashBits = 12
blockhashLength = blockhashBits ^ 2 `div` 4

getHashImageInfo :: FilePath -> Either String ImageInfo
getHashImageInfo path = do
  dashIndex <- maybe reportError
    Right . findIndex (=='-') $ basename
  let (contentString, '-':perceptualString) = splitAt dashIndex basename

  contentHash <- if all isHexDigit contentString && length contentString == 16
    then Right . FnvHash64 . fst . head . readHex $ contentString
    else reportError
  perceptualHash <- if all isHexDigit perceptualString && length perceptualString == blockhashLength
    then Right . Hash . V.fromList $ perceptualString
    else reportError


  return $ ImageInfo path contentHash perceptualHash
  where
    basename = takeBaseName path
    reportError = Left $ "Error decoding hash from image path " ++ path

getImageInfo :: MonadIO m => FilePath -> m (Either String ImageInfo)
getImageInfo path = do
  bytes <- liftIO $ BS.readFile path
  return $ case P.convertRGBA8 <$> P.decodeImage bytes of
    Left err -> Left err
    Right (P.Image width height pixels) -> Right $ ImageInfo
      path
      (fnv1a_64Hash bytes)
      (blockhash (Image width height (V.convert pixels)) blockhashBits Precise)

data ImageInfo = ImageInfo
  { path           :: FilePath
  , contentHash    :: FnvHash64
  , perceptualHash :: Hash
  } deriving (Show)

hashbasedFilename :: MonadReader Config m => ImageInfo -> m FilePath
hashbasedFilename ImageInfo { path, perceptualHash, contentHash = FnvHash64 contentHashWord } = do
  hashdir <- asks hashdir
  return $ hashdir </> (printf "%016x" contentHashWord ++ "-" ++ show perceptualHash ++ takeExtension path)


data ImageDifference = Same -- Same content hash
                     | Similar -- Similar or same perceptual hash, different content hash
                     | Different -- Very different perceptual hash, different content hash
                     deriving (Ord, Eq)

compareImages :: Int -> ImageInfo -> ImageInfo -> ImageDifference
compareImages similarDist left right
  | contentHash left == contentHash right = Same
  | perceptualHash left `hammingDistance` perceptualHash right < similarDist = Similar
  | otherwise = Different
