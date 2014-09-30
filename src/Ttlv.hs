{-# LANGUAGE OverloadedStrings #-}
module Ttlv ( encodeTtlv
            , decodeTtlv
            , Ttlv
            , TtlvData
            , TtlvTag ) where

import Control.Applicative
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
-- import Data.Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import System.Locale
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTime)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Base16 as B16
import Numeric (showHex)
import qualified Data.ByteString.Internal as BS (w2c, c2w)
import Test.Hspec
import Data.Maybe
import Control.Lens

import Ttlv.Tag
import Ttlv.Data
import qualified Ttlv.Enum as TEnum

instance Binary Ttlv where
  get = parseTtlv
  put = unparseTtlv

parseTtlvStructure' :: Get [Ttlv]
parseTtlvStructure' = do
  ttlv <- parseTtlv
  rema <- remaining
  if rema == 0
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

-- FIXME this isn't implemented
parseTtlvBigInt :: Get TtlvData
parseTtlvBigInt = do
  val <- getRemainingLazyByteString
--  return $ TtlvBigInt $ fromIntegral val
  return $ TtlvBigInt 0

parseTtlvEnum :: Get TtlvData
parseTtlvEnum = do
  val <- getWord32be
  return $ TtlvEnum $ fromIntegral val

parseTtlvBool :: Get TtlvData
parseTtlvBool = do
  val <- getWord64be
  return $ TtlvBool (val == 1)

-- FIXME this isn't implemented
parseTtlvString :: Get TtlvData
parseTtlvString = do
  val <- getRemainingLazyByteString
  return $ TtlvString $ map BS.w2c $ L.unpack val

parseTtlvByteString :: Get TtlvData
parseTtlvByteString = do
  val <- getRemainingLazyByteString
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

prettyPrint :: L.ByteString -> String
prettyPrint = concat . map (flip showHex "-") . L.unpack

parseTtlv :: Get Ttlv
parseTtlv = do
  tag <- getLazyByteString 3
  typ <- getWord8
  len <- getWord32be
  val <- getLazyByteString $ fromIntegral len
  when (fromIntegral typ `elem` [2, 5, 10]) $ skip 4 -- skip padding
  return $ Ttlv (toTtlvTag $ decodeTtlvTag tag)
             (case fromIntegral typ of
                1  -> runGet parseTtlvStructure val
                2  -> runGet parseTtlvInt val
                3  -> runGet parseTtlvLongInt val
                4  -> runGet parseTtlvBigInt val
                5  -> runGet parseTtlvEnum val
                6  -> runGet parseTtlvBool val
                7  -> runGet parseTtlvString val
                8  -> runGet parseTtlvByteString val
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
ttlvDataLength (TtlvStructure s) = foldr1 (+) $ map ttlvLength s
ttlvDataLength (TtlvInt _) = 8 -- w/padding
ttlvDataLength (TtlvLongInt _) = 8
ttlvDataLength (TtlvBigInt _) = undefined
ttlvDataLength (TtlvEnum _) = 8 -- w/padding
ttlvDataLength (TtlvBool _) = 8
ttlvDataLength (TtlvString x) = length x
ttlvDataLength (TtlvByteString x) = fromIntegral $ L.length x
ttlvDataLength (TtlvDateTime _) = 8
ttlvDataLength (TtlvInterval _) = 8 -- w/padding

ttlvLength :: Ttlv -> Int
ttlvLength (Ttlv t d) = 3 + 1 + 4 + (ttlvDataLength d)

unparseTtlvData :: TtlvData -> Put
unparseTtlvData (TtlvStructure x) = mapM_ unparseTtlv x
unparseTtlvData (TtlvInt x) = do
  putWord32be $ fromIntegral x
  putWord32be 0
unparseTtlvData (TtlvLongInt x) = putWord64be $ fromIntegral x
-- unparseTtlvdata (TtlvBigInt x) = fail "Undefined encode for Big Integer"
unparseTtlvData (TtlvEnum x) = do
  putWord32be $ fromIntegral x
  putWord32be 0
unparseTtlvData (TtlvBool x) = if x
                               then putWord64be 1
                               else putWord64be 0
unparseTtlvData (TtlvString x) = putLazyByteString $ L.pack $ map BS.c2w x 
unparseTtlvData (TtlvByteString x) = putLazyByteString x
unparseTtlvData (TtlvDateTime x) = putWord64be $ fromIntegral $ round $ utcTimeToPOSIXSeconds x
unparseTtlvData (TtlvInterval x) = do
  putWord32be $ fromIntegral x
  putWord32be 0

unparseTtlv :: Ttlv -> Put
unparseTtlv (Ttlv t d) = do
  putLazyByteString $ encodeTtlvTag $ fromTtlvTag t
  putWord8 $ fromIntegral $ ttlvDataType d
  -- this is terrible. Find a better way to do this
  let length = ttlvDataLength d
      real_length = if ttlvDataType d `elem` [2, 5, 10]
                    then length - 4
                    else length
  putWord32be $ fromIntegral $ real_length
  unparseTtlvData d

protocolVersion :: Int -> Int -> Ttlv
protocolVersion major minor = Ttlv TtlvProtocolVersion (TtlvStructure [ Ttlv TtlvProtocolVersionMajor (TtlvInt major)
                                                                      , Ttlv TtlvProtocolVersionMinor (TtlvInt minor)])

-- validate :: Ttlv -> Bool

validateProtocolVersion :: Ttlv -> Bool
validateProtocolVersion (Ttlv TtlvProtocolVersion (TtlvStructure [Ttlv TtlvProtocolVersionMajor _, Ttlv TtlvProtocolVersionMinor _])) = True
validateProtocolVersion _ = False

-- validateEnum :: Ttlv -> Bool
-- validateEnum (Ttlv t d) = 

fromHex x = L.fromChunks [fst $ B16.decode x]

exampleTtlvs = [ "42002002000000040000000800000000" -- Integer
               , "420020030000000801B69B4BA5749200" -- Long Integer
               , "42002004000000100000000003FD35EB6BC2DF4618080000" --BigInt
               , "4200200500000004000000FF00000000" -- Enum
               , "42002006000000080000000000000001" -- Bool
               , "420020070000000B48656C6C6F20576F726C640000000000" -- String
               , "42002008000000030102030000000000" -- Byte String
               , "42002009000000080000000047DA67F8" -- Date Time
               , "4200200A00000004000D2F0000000000" -- Interval
               , "42002001000000204200040500000004000000FE000000004200050200000004000000FF00000000" -- Structure
               ]
testTtlvs = map fromHex exampleTtlvs

-- | Decode a Lazy ByteString into the corresponding Ttlv type
decodeTtlv :: L.ByteString -> Ttlv
decodeTtlv = runGet parseTtlv


encodeTtlv :: Ttlv -> L.ByteString
encodeTtlv x = runPut $ unparseTtlv x
  
test :: IO ()
test = do
  hspec $ do
    describe "Ttlv" $ do
      describe "Examples" $ do
        describe "Encode" $ do
          it "should encode/decode Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvInt 8))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Long Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvLongInt 123456789000000000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Big Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvBigInt 1234567890000000000000000000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Enumeration" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvEnum 255))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Boolean" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvBool True))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode String" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode ByteString" $ do
            pending
          it "should encode/decode DateTime" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Interval" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvInterval 864000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Structure" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvStructure [Ttlv TtlvApplicationSpecificInformation (TtlvEnum 254),Ttlv TtlvArchiveDate (TtlvInt 255)]))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
            
        describe "Decode" $ do
          it "should decode Integer" $ do
            decodeTtlv (testTtlvs !! 0) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvInt 8))
          it "should decode Long Integer" $ do
            decodeTtlv (testTtlvs !! 1) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvLongInt 123456789000000000))
          it "should decode Big Integer" $ do
            decodeTtlv (testTtlvs !! 2) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvBigInt 1234567890000000000000000000))
          it "should decode Enumeration" $ do
            decodeTtlv (testTtlvs !! 3) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvEnum 255))
          it "should decode Boolean" $ do
            decodeTtlv (testTtlvs !! 4) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvBool True))
          it "should decode String" $ do
            decodeTtlv (testTtlvs !! 5) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
          it "should decode ByteString" $ do
            pending
            -- decodeTtlv (testTtlvs !! 6) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvByteString undefined))
          it "should decode DateTime" $ do
            decodeTtlv (testTtlvs !! 7) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvDateTime (fromJust $ parseTime defaultTimeLocale "%F %T %Z" "2008-03-14 11:56:40 UTC")))
          it "should decode Interval" $ do
            decodeTtlv (testTtlvs !! 8) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvInterval 864000))
          it "should decode Structure" $ do
            decodeTtlv (testTtlvs !! 9) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvStructure [Ttlv TtlvApplicationSpecificInformation (TtlvEnum 254), Ttlv TtlvArchiveDate (TtlvInt 255)]))
      describe "validation" $ do
        it "should validate protocol version" $ do
          validateProtocolVersion (protocolVersion 1 0) `shouldBe` True
          validateProtocolVersion (protocolVersion 1 1) `shouldBe` True
          validateProtocolVersion (protocolVersion 1 2) `shouldBe` True
          validateProtocolVersion (Ttlv TtlvProtocolVersion (TtlvStructure [Ttlv TtlvCompromiseDate (TtlvInt 1), Ttlv TtlvProtocolVersionMinor (TtlvInt 0)])) `shouldBe` False
          validateProtocolVersion (Ttlv TtlvProtocolVersion (TtlvStructure [Ttlv TtlvProtocolVersionMajor (TtlvInt 1), Ttlv TtlvProtocolVersionMinor (TtlvBool True)])) `shouldBe` True


main :: IO ()
main = do
  mapM_ (putStrLn . show . decodeTtlv) testTtlvs

