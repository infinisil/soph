{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Hashing where

import qualified Codec.Picture          as P
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.Blockhash
import           Data.ByteArray.Hash    (FnvHash64 (..), fnv1a_64Hash)
import qualified Data.ByteString        as BS
import qualified Data.Vector.Unboxed    as V

import           Config
import           Data.Char              (isHexDigit)
import           Data.List              (elemIndex)
import           Numeric                (readHex)
import           System.FilePath
import           Text.Printf            (printf)

instance Eq Hash where
  (Hash l) == (Hash r) = l == r

blockhashBits = 12
blockhashLength = blockhashBits ^ 2 `div` 4

getHashImageInfo :: FilePath -> Either String ImageInfo
getHashImageInfo path = do
  dashIndex <- maybe reportError
    Right . elemIndex '-' $ basename
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
