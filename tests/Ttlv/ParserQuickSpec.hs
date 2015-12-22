{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Ttlv.ParserQuickSpec where

import           Ttlv.Data
import           Ttlv.Parser.Binary
import qualified Ttlv.Tag               as T

import           Test.Hspec

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe
import           Data.Time
import qualified Data.Text              as Text (pack)

import           Test.QuickCheck
import           Test.QuickCheck.Classes

instance Arbitrary T.TtlvTag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary T.Tag where
  arbitrary = frequency [ (8, T.Tag <$> arbitrary)
                        , (1, T.Extension <$> choose (0x540000, 0x54FFFF))
                        , (1, T.Unknown <$> suchThat arbitrary (\x -> (x >= 0 && x <= 0x420000) ||
                                                                      x >= 0x550000))]

instance Arbitrary Ttlv where
  arbitrary = Ttlv <$> arbitrary <*> arbitrary

instance Arbitrary TtlvData where
  arbitrary = frequency [ (1, TtlvStructure <$> vector 10) -- limit to size 10 to prevent deep recursive generation
                        , (2, TtlvInt <$> arbitrary) -- FIXME negative
                        , (2, TtlvLongInt <$> arbitrary) -- FIXME negative
                        , (2, TtlvBigInt <$> suchThat arbitrary (> 0)) -- FIXME zero/negative
                        , (2, TtlvEnum <$> arbitrary)
                        , (2, TtlvBool <$> arbitrary)
                        , (2, TtlvString <$> return "qc")
                        , (2, TtlvByteString <$> return "qc")
                        , (2, TtlvInterval <$> arbitrary)]

spec :: Spec
spec = do
  describe "Ttlv" $ do
    describe "Properties" $ do
      it "TtlvTag isomorphism" $ do
        property $ \x -> (T.toTtlvTag . T.fromTtlvTag) x == (x :: T.TtlvTag)
      it "Tag isomorphism" $ do
        property $ \x -> (T.toTag . T.fromTag) x == (x :: T.Tag)
      it "Tag reverse isomorphism" $ do
        property $ \x -> (T.fromTag . T.toTag) x == (x :: Int)

      it "Parser isomorphism" $ do
        property $ \x -> (decodeTtlv . encodeTtlv) x == Right (x :: Ttlv)
