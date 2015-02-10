{-# LANGUAGE OverloadedStrings #-}
module Ttlv.Parser ( encodeTtlv
                   , decodeTtlv
                   , validateProtocolVersion
                   , protocolVersion ) where

import Debug.Trace (trace)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BS (w2c, c2w)
import qualified Crypto.Number.Serialize as CN (os2ip, i2osp, i2ospOf_, lengthBytes)
import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.Monad

import Ttlv.Tag
import Ttlv.Data
import qualified Ttlv.Enum as E

instance Binary Ttlv where
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
  -- rea <- remaining
  -- trace ("PARSE STRUCT:" ++ show rea) remaining
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

-- FIXME this isn't implemented
parseTtlvBigInt :: Int -> Get TtlvData
parseTtlvBigInt n = do
  val <- getLazyByteString $ fromIntegral n
  return $ TtlvBigInt $ CN.os2ip $ L.toStrict val

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
  val <- getLazyByteString $ fromIntegral n
  return $ TtlvString $ map BS.w2c $ L.unpack val

parseTtlvByteString :: Int -> Get TtlvData
parseTtlvByteString n = do
  val <- getLazyByteString $ fromIntegral n
  return $ TtlvByteString val

parseTtlvDateTime :: Get TtlvData
parseTtlvDateTime = do
  val <- getWord64be
  return $ TtlvDateTime $ posixSecondsToUTCTime $ fromIntegral val

parseTtlvInterval :: Get TtlvData
parseTtlvInterval = do
  val <- getWord32be
  return $ TtlvInterval $ fromIntegral val

decodeTtlvTag :: L.ByteString -> Int
decodeTtlvTag x = fromIntegral (decode (0 `L.cons'` x) :: Word32)

encodeTtlvTag :: Int -> L.ByteString
encodeTtlvTag x = snd $ fromJust $ L.uncons $ encode (fromIntegral x :: Word32)

parseTtlv :: Get Ttlv
parseTtlv = do
  -- trace ("--------------------------------------") remaining
  -- rea <- remaining
  -- trace ("Full:" ++ show rea) remaining
  tag <- getLazyByteString 3
  -- trace ("Tag:" ++ (show $ toHex tag)) remaining
  typ <- getWord8
  -- trace ("Typ:" ++ show typ) remaining
  len <- getWord32be
  -- trace ("Len:" ++ show len) remaining
  -- rea <- remaining
  -- trace ("Rem:" ++ show rea) remaining
  val <- getLazyByteString $ fromIntegral len
  -- trace ("VAL: " ++ (show $ toHex val)) remaining
  -- 8-byte/64-bit alignment for 
  let skipBytes = (8 - (len `rem` 8)) `rem` 8 -- FIXME maybe??
  when (skipBytes /= 0 && (fromIntegral typ `elem` [2, 5, 7, 8, 10])) $
    skip $ fromIntegral skipBytes
  -- trace ("Skip bytes:" ++ show skipBytes) remaining
  -- rea <- remaining
  -- trace ("XRem:" ++ show rea) remaining
  -- trace ("Len:" ++ show len) remaining
  -- trace ("Val Len:" ++ show (fromIntegral $ L.length val)) remaining
  return $ Ttlv (toTtlvTag $ decodeTtlvTag tag)
    (case fromIntegral typ of
        1  -> runGet parseTtlvStructure val
        2  -> runGet parseTtlvInt val
        3  -> runGet parseTtlvLongInt val
        4  -> runGet (parseTtlvBigInt $ fromIntegral len) val
        5  -> runGet parseTtlvEnum val
        6  -> runGet parseTtlvBool val
        7  -> runGet (parseTtlvString $ fromIntegral len) val
        8  -> runGet (parseTtlvByteString $ fromIntegral len) val
        9  -> runGet parseTtlvDateTime val
        10 -> runGet parseTtlvInterval val
        otherwise -> error "unknown type")

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
ttlvDataLength (TtlvStructure x) = if length x == 0
                                   then 0 -- empty structure
                                   else foldr1 (+) $ map ttlvLength x
ttlvDataLength (TtlvInt _) = 4 -- w/o padding
ttlvDataLength (TtlvLongInt _) = 8
ttlvDataLength (TtlvBigInt x) = CN.lengthBytes x -- w/o padding
ttlvDataLength (TtlvEnum _) = 4 -- w/o padding
ttlvDataLength (TtlvBool _) = 8
ttlvDataLength (TtlvString x) = length x -- w/o padding
ttlvDataLength (TtlvByteString x) = fromIntegral $ L.length x -- w/o padding
ttlvDataLength (TtlvDateTime _) = 8
ttlvDataLength (TtlvInterval _) = 4 -- w/o padding

ttlvLength :: Ttlv -> Int
ttlvLength (Ttlv t d) = 3 + 1 + 4 + (paddedTtlvDataLength d)

-- put data without padding
unparseTtlvData :: TtlvData -> Put
unparseTtlvData (TtlvStructure x) = mapM_ unparseTtlv x
unparseTtlvData (TtlvInt x) = do
  putWord32be $ fromIntegral x
unparseTtlvData (TtlvLongInt x) = putWord64be $ fromIntegral x
unparseTtlvData z@(TtlvBigInt x) = putByteString $ CN.i2ospOf_ (ttlvDataLength z) x
unparseTtlvData (TtlvEnum x) = do
  putWord32be $ fromIntegral x
unparseTtlvData (TtlvBool x) = if x
                               then putWord64be 1
                               else putWord64be 0
unparseTtlvData (TtlvString x) = putLazyByteString $ L.pack $ map BS.c2w x 
unparseTtlvData (TtlvByteString x) = putLazyByteString x
unparseTtlvData (TtlvDateTime x) = putWord64be $ fromIntegral $ round $ utcTimeToPOSIXSeconds x
unparseTtlvData (TtlvInterval x) = do
  putWord32be $ fromIntegral x

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
  putLazyByteString $ encodeTtlvTag $ fromTtlvTag t
  putWord8 $ fromIntegral $ ttlvDataType d
  -- this is terrible. Find a better way to do this
  let length = ttlvDataLength d
      realLength = paddedTtlvDataLength d
  --trace ("Length: " ++ show length ++ " Real Length:" ++ show realLength) (return ())
  putWord32be $ fromIntegral length
  unparseTtlvData d
  -- add padding at end
  replicateM_ (realLength - length) (putWord8 0)


protocolVersion :: Int -> Int -> Ttlv
protocolVersion major minor = Ttlv TtlvProtocolVersion (TtlvStructure [ Ttlv TtlvProtocolVersionMajor (TtlvInt major)
                                                                      , Ttlv TtlvProtocolVersionMinor (TtlvInt minor)])

-- validate :: Ttlv -> Bool

validateProtocolVersion :: Ttlv -> Bool
validateProtocolVersion (Ttlv TtlvProtocolVersion (TtlvStructure [Ttlv TtlvProtocolVersionMajor _, Ttlv TtlvProtocolVersionMinor _])) = True
validateProtocolVersion _ = False

-- validateEnum :: Ttlv -> Bool
-- validateEnum (Ttlv t d) = 

-- | Decode a Lazy ByteString into the corresponding Ttlv type
decodeTtlv :: L.ByteString -> Ttlv
decodeTtlv = runGet parseTtlv


encodeTtlv :: Ttlv -> L.ByteString
encodeTtlv x = runPut $ unparseTtlv x
