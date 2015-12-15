{-# LANGUAGE OverloadedStrings #-}
module Ttlv.Parser.Serialize ( encodeTtlv
                             , decodeTtlv
                             , decodeTtlvLazy
                             ) where

import           Control.Monad
import qualified Crypto.Number.Serialize  as CN (i2osp, i2ospOf_, lengthBytes,
                                                 os2ip)
import           Data.Serialize
import           Data.Serialize.Get
import           Data.Serialize.Put
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy     as L
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text                as T
import           Data.Word

import           Ttlv.Data
import qualified Ttlv.Enum                as E
import           Ttlv.Tag

instance Serialize Ttlv where
  get = parseTtlv
  put = unparseTtlv

parseTtlvStructure' :: Get [Ttlv]
parseTtlvStructure' = do
  empty <- isEmpty
  if empty -- handle empty structure
  then return []
  else do
    ttlv <- parseTtlv
    empty <- isEmpty
    if empty
    then return [ttlv]
    else do
      rest <- parseTtlvStructure'
      return $ ttlv : rest

parseTtlvStructure :: Get TtlvData
parseTtlvStructure = do
  ttlvs <- parseTtlvStructure'
  return $ TtlvStructure ttlvs

parseTtlvInt :: Get TtlvData
parseTtlvInt = do
  val <- getWord32be
  return $ TtlvInt $ fromIntegral val

parseTtlvLongInt :: Get TtlvData
parseTtlvLongInt = do
  val <- getWord64be
  return $ TtlvLongInt $ fromIntegral val

-- FIXME this doesn't handle negative numbers
parseTtlvBigInt :: Int -> Get TtlvData
parseTtlvBigInt n = do
  val <- getByteString $ fromIntegral n
  return $ TtlvBigInt $ CN.os2ip val

parseTtlvEnum :: Get TtlvData
parseTtlvEnum = do
  val <- getWord32be
  return $ TtlvEnum $ fromIntegral val

parseTtlvBool :: Get TtlvData
parseTtlvBool = do
  val <- getWord64be
  return $ TtlvBool (val == 1)

parseTtlvString :: Int -> Get TtlvData
parseTtlvString n = do
  val <- getByteString $ fromIntegral n
  return $ TtlvString $ decodeUtf8 val

parseTtlvByteString :: Int -> Get TtlvData
parseTtlvByteString n = do
  val <- getByteString $ fromIntegral n
  return $ TtlvByteString val

parseTtlvDateTime :: Get TtlvData
parseTtlvDateTime = do
  val <- getWord64be
  return $ TtlvDateTime $ posixSecondsToUTCTime $ fromIntegral val

parseTtlvInterval :: Get TtlvData
parseTtlvInterval = do
  val <- getWord32be
  return $ TtlvInterval $ fromIntegral val

decodeTtlvTag :: B.ByteString -> Int
decodeTtlvTag x = fromIntegral tag
  where Right tag = runGet getWord32be (0 `B.cons` x)

encodeTtlvTag :: Int -> B.ByteString
encodeTtlvTag x = snd $ fromJust $ B.uncons $ encode (fromIntegral x :: Word32)

parseTtlv :: Get Ttlv
parseTtlv = do
  tag <- getByteString 3
  typ <- getWord8
  len <- getWord32be
  val <- getByteString $ fromIntegral len
  let skipBytes = (8 - (len `rem` 8)) `rem` 8 -- FIXME maybe??
  when (skipBytes /= 0 && (fromIntegral typ `elem` [2, 5, 7, 8, 10])) $
    skip $ fromIntegral skipBytes
  let valParser = case fromIntegral typ of
        1  -> parseTtlvStructure
        2  -> parseTtlvInt
        3  -> parseTtlvLongInt
        4  -> (parseTtlvBigInt $ fromIntegral len)
        5  -> parseTtlvEnum
        6  -> parseTtlvBool
        7  -> (parseTtlvString $ fromIntegral len)
        8  -> (parseTtlvByteString $ fromIntegral len)
        9  -> parseTtlvDateTime
        10 -> parseTtlvInterval
        _  -> error "unknown type"
  case runGet valParser val of
    Right ttlvData -> return $ Ttlv (toTag $ decodeTtlvTag tag) ttlvData
    Left e -> fail e

-- | Retrieve the corresponding ID for TtlvData
ttlvDataType :: TtlvData -> Int
ttlvDataType (TtlvStructure _) = 1
ttlvDataType (TtlvInt _) = 2
ttlvDataType (TtlvLongInt _) = 3
ttlvDataType (TtlvBigInt _) = 4
ttlvDataType (TtlvEnum _) = 5
ttlvDataType (TtlvBool _) = 6
ttlvDataType (TtlvString _) = 7
ttlvDataType (TtlvByteString _) = 8
ttlvDataType (TtlvDateTime _) = 9
ttlvDataType (TtlvInterval _) = 10

ttlvDataLength :: TtlvData -> Int
ttlvDataLength (TtlvStructure x) = if null x
                                   then 0 -- empty structure
                                   else sum $ map ttlvLength x
ttlvDataLength (TtlvInt _) = 4 -- w/o padding
ttlvDataLength (TtlvLongInt _) = 8
ttlvDataLength (TtlvBigInt x) = CN.lengthBytes x -- w/o padding
ttlvDataLength (TtlvEnum _) = 4 -- w/o padding
ttlvDataLength (TtlvBool _) = 8
ttlvDataLength (TtlvString x) = fromIntegral $ B.length $ encodeUtf8 $ x -- w/o padding
ttlvDataLength (TtlvByteString x) = fromIntegral $ B.length x -- w/o padding
ttlvDataLength (TtlvDateTime _) = 8
ttlvDataLength (TtlvInterval _) = 4 -- w/o padding

ttlvLength :: Ttlv -> Int
ttlvLength (Ttlv _ d) = 3 + 1 + 4 + paddedTtlvDataLength d

-- put data without padding
unparseTtlvData :: TtlvData -> Put
unparseTtlvData (TtlvStructure x) = mapM_ unparseTtlv x
unparseTtlvData (TtlvInt x) = putWord32be $ fromIntegral x
unparseTtlvData (TtlvLongInt x) = putWord64be $ fromIntegral x
unparseTtlvData z@(TtlvBigInt x) = putByteString $ CN.i2ospOf_ (ttlvDataLength z) x
unparseTtlvData (TtlvEnum x) = putWord32be $ fromIntegral x
unparseTtlvData (TtlvBool x) = if x
                               then putWord64be 1
                               else putWord64be 0
unparseTtlvData (TtlvString x) = putByteString $ encodeUtf8 x
unparseTtlvData (TtlvByteString x) = putByteString x
unparseTtlvData (TtlvDateTime x) = putWord64be $ fromIntegral $ round $ utcTimeToPOSIXSeconds x
unparseTtlvData (TtlvInterval x) = putWord32be $ fromIntegral x

paddedTtlvDataLength :: TtlvData -> Int
paddedTtlvDataLength (TtlvInt _) = 8
paddedTtlvDataLength (TtlvEnum _) = 8
paddedTtlvDataLength (TtlvInterval _) = 8
paddedTtlvDataLength x@(TtlvString _) = let n = ttlvDataLength x
                                        in n + (8 - (n `rem` 8)) `rem` 8
paddedTtlvDataLength x@(TtlvByteString _) = let n = ttlvDataLength x
                                            in n + (8 - (n `rem` 8)) `rem` 8
paddedTtlvDataLength x = ttlvDataLength x

unparseTtlv :: Ttlv -> Put
unparseTtlv (Ttlv t d) = do
  putByteString $ encodeTtlvTag $ fromTag t
  putWord8 $ fromIntegral $ ttlvDataType d
  -- this is terrible. Find a better way to do this
  let len = ttlvDataLength d
      realLength = paddedTtlvDataLength d
  putWord32be $ fromIntegral len
  unparseTtlvData d
  -- add padding at end
  replicateM_ (realLength - len) (putWord8 0)


-- | Decode a ByteString into the corresponding Ttlv type
decodeTtlv :: B.ByteString -> Either String Ttlv
decodeTtlv = runGet parseTtlv

decodeTtlvLazy :: L.ByteString -> Either String Ttlv
decodeTtlvLazy = runGetLazy parseTtlv

encodeTtlv :: Ttlv -> B.ByteString
encodeTtlv x = runPut $ unparseTtlv x
