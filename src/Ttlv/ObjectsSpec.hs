module Ttlv.ObjectsSpec where

import Test.Hspec

import Ttlv.Data
import Ttlv.Tag
import Ttlv.Objects
import Ttlv.Structures

spec :: Spec
spec = do
  describe "Validator" $ do
    it "should sub-apply validators" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
      runTtlvParser x xx `shouldBe` Right xx
    it "should allow chaining of validators" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
      let y = apply TtlvAttributeValue ok
      runTtlvParser (x <+> y) xx `shouldBe` Right xx
    it "should allow chaining of validators (alternation)" $ do
      let x = apply TtlvAttributeName (stringEq "x-hi")
          y = apply TtlvAttributeName (stringEq "x-yo")
          p = x <|> y
      runTtlvParser p xx `shouldBe` Right xx
      runTtlvParser p yy `shouldBe` Right yy
    it "should fail bad matches" $ do
      let x = apply TtlvAttributeName (stringEq "abc")
      runTtlvParser x xx `shouldBe` Left ["mismatch in expected value"]
    it "should allow many (one-many)" $ do
      let xx' = (Ttlv TtlvAttribute (TtlvStructure [xx, yy]))
      runTtlvParser (many1 TtlvAttribute attribute_) xx' `shouldBe` Right xx'
      runTtlvParser (many1 TtlvArchiveDate attribute_) xx' `shouldBe` Left ["unable to find tag TtlvArchiveDate"]
    it "should allow many (zero-many)" $ do
      let xx' = (Ttlv TtlvAttribute (TtlvStructure [xx, yy]))
      runTtlvParser (many TtlvAttribute attribute_) xx' `shouldBe` Right xx'
      runTtlvParser (many TtlvArchiveDate attribute_) xx' `shouldBe` Right xx'

    describe "Objects" $ do
      describe "Attribute" $ do
        it "valid" $ do
          runTtlvParser (attribute "x-hi" string) xx `shouldBe` Right xx
        it "unexpected attribute" $ do
          runTtlvParser (attribute "x-hi" tint) xx `shouldBe` Left ["failed combinator", "not an int"]
      describe "Credential" $ do
        it "valid" $ do
          runTtlvParser credential zz `shouldBe` Right zz
