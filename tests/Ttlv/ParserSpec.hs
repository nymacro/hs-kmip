{-# LANGUAGE OverloadedStrings #-}
module Ttlv.ParserSpec where

import           Ttlv.Data
import           Ttlv.Parser
import qualified Ttlv.Tag               as T

import           Test.Hspec

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe
import           Data.Time

fromHex :: B.ByteString -> L.ByteString
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

testTtlvs :: [L.ByteString]
testTtlvs = map fromHex exampleTtlvs

test :: IO ()
test = hspec spec

ttlv :: T.TtlvTag -> TtlvData -> Ttlv
ttlv t d = Ttlv (T.Tag t) d

spec :: Spec
spec = do
  describe "Ttlv" $ do
    describe "Examples" $ do
      describe "Encode" $ do
        it "should encode/decode Integer" $ do
          let t = ttlv T.CompromiseDate (TtlvInt 8)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Long Integer" $ do
          let t = ttlv T.CompromiseDate (TtlvLongInt 123456789000000000)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Big Integer" $ do
          let t = ttlv T.CompromiseDate (TtlvBigInt 1234567890000000000000000000)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Big Integer" $ do
          let t = ttlv T.CompromiseDate (TtlvBigInt (-1))
          pending
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Enumeration" $ do
          let t = ttlv T.CompromiseDate (TtlvEnum 255)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Boolean" $ do
          let t = ttlv T.CompromiseDate (TtlvBool True)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode String" $ do
          let t = ttlv T.CompromiseDate (TtlvString "Hello World")
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode ByteString" $ do
          let t = ttlv T.CompromiseDate (TtlvByteString $ fromHex "010203")
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode DateTime" $ do
          let t = ttlv T.CompromiseDate (TtlvString "Hello World")
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Interval" $ do
          let t = ttlv T.CompromiseDate (TtlvInterval 864000)
          (decodeTtlv $ encodeTtlv t) `shouldBe` t
        it "should encode/decode Structure" $ do
          let t = ttlv T.CompromiseDate (TtlvStructure [ttlv T.ApplicationSpecificInformation (TtlvEnum 254), ttlv T.ArchiveDate (TtlvInt 255)])
          (decodeTtlv $ encodeTtlv t) `shouldBe` t

      describe "Decode" $ do
        it "should decode Integer" $ do
          decodeTtlv (testTtlvs !! 0) `shouldBe` (ttlv T.CompromiseDate (TtlvInt 8))
        it "should decode Long Integer" $ do
          decodeTtlv (testTtlvs !! 1) `shouldBe` (ttlv T.CompromiseDate (TtlvLongInt 123456789000000000))
        it "should decode Big Integer" $ do
          decodeTtlv (testTtlvs !! 2) `shouldBe` (ttlv T.CompromiseDate (TtlvBigInt 1234567890000000000000000000))
        it "should decode Enumeration" $ do
          decodeTtlv (testTtlvs !! 3) `shouldBe` (ttlv T.CompromiseDate (TtlvEnum 255))
        it "should decode Boolean" $ do
          decodeTtlv (testTtlvs !! 4) `shouldBe` (ttlv T.CompromiseDate (TtlvBool True))
        it "should decode String" $ do
          decodeTtlv (testTtlvs !! 5) `shouldBe` (ttlv T.CompromiseDate (TtlvString "Hello World"))
        it "should decode ByteString" $ do
          decodeTtlv (testTtlvs !! 6) `shouldBe` (ttlv T.CompromiseDate (TtlvByteString (fromHex "010203")))
        it "should decode DateTime" $ do
          decodeTtlv (testTtlvs !! 7) `shouldBe` (ttlv T.CompromiseDate (TtlvDateTime (fromJust $ parseTime defaultTimeLocale "%F %T %Z" "2008-03-14 11:56:40 UTC")))
        it "should decode Interval" $ do
          decodeTtlv (testTtlvs !! 8) `shouldBe` (ttlv T.CompromiseDate (TtlvInterval 864000))
        it "should decode Structure" $ do
          decodeTtlv (testTtlvs !! 9) `shouldBe` (ttlv T.CompromiseDate (TtlvStructure [ttlv T.ApplicationSpecificInformation (TtlvEnum 254), ttlv T.ArchiveDate (TtlvInt 255)]))
