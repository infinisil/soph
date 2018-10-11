{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Hashing where

import qualified Codec.Picture        as P
import           Control.Applicative  (liftA2)
import           Control.Monad.Reader
import           Data.Blockhash
import           Data.ByteArray.Hash  (FnvHash64 (..), fnv1a_64Hash)
import qualified Data.ByteString      as BS
import           Data.Char            (isHexDigit)
import           Data.List            (elemIndex)
import           Data.List.NonEmpty   (NonEmpty (..), toList)
import qualified Data.Vector.Unboxed  as V
import           Numeric              (readHex)
import           System.FilePath
import           Text.Printf          (printf)

import qualified Data.Set.BKTree      as BK

import           Config

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

  return $ ImageInfo (takeExtension path) contentHash perceptualHash
  where
    basename = takeBaseName path
    reportError = Left $ "Error decoding hash from image path " ++ path

getImageInfo :: MonadIO m => FilePath -> m (Either String ImageInfo)
getImageInfo path = do
  bytes <- liftIO $ BS.readFile path
  return $ getImageInfoBS (takeExtension path) bytes

getImageInfoBS :: String -> BS.ByteString -> Either String ImageInfo
getImageInfoBS extension bytes = case P.convertRGBA8 <$> P.decodeImage bytes of
  Left err -> Left err
  Right (P.Image width height pixels) -> Right $ ImageInfo
    extension
    (fnv1a_64Hash bytes)
    (blockhash (Image width height (V.convert pixels)) blockhashBits Precise)

data ImageInfo = ImageInfo
  { extension      :: String
  , contentHash    :: FnvHash64
  , perceptualHash :: Hash
  } deriving (Show, Eq)

hashbasedFilename :: MonadReader Config m => ImageInfo -> m FilePath
hashbasedFilename ImageInfo { extension, perceptualHash, contentHash = FnvHash64 contentHashWord } = do
  hashdir <- asks hashdir
  return $ hashdir </> (printf "%016x" contentHashWord ++ "-" ++ show perceptualHash ++ extension)


-- TODO: Replace with BKTree (bktrees package)

search :: BK.BKTree ImageInfo -> ImageInfo -> SearchResult
search database query
  | BK.member query database = Present
  | otherwise = case BK.elemsDistance 10 query database of
    []     -> NotPresent
    (x:xs) -> SimilarPictures (x :| xs)

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

-- TODO: Does this satisfy all metric properties?
instance BK.Metric ImageInfo where
  distance left right = if contentHash left == contentHash right then 0
    else 1 + (perceptualHash left `hammingDistance` perceptualHash right)


compareImages :: Int -> ImageInfo -> ImageInfo -> ImageDifference
compareImages similarDist left right
  | contentHash left == contentHash right = Same
  | perceptualHash left `hammingDistance` perceptualHash right < similarDist = Similar
  | otherwise = Different
