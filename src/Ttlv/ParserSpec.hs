{-# LANGUAGE OverloadedStrings #-}
module Ttlv.ParserSpec where

import Ttlv.Data
import Ttlv.Tag
import Ttlv.Parser

import Test.Hspec

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base16 as B16

fromHex x = L.fromChunks [fst $ B16.decode x]
toHex x = map B16.encode (L.toChunks x)

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

test :: IO ()
test = hspec spec

spec :: Spec
spec = do
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
        it "should encode/decode Big Integer" $ do
          let t = (Ttlv TtlvCompromiseDate (TtlvBigInt (-1)))
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

-- main :: IO ()
-- main = do
--   mapM_ (putStrLn . show . decodeTtlv) testTtlvs
