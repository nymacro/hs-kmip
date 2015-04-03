module Ttlv.ObjectsSpec where

import Test.Hspec

import Ttlv.Data
import qualified Ttlv.Tag as T
import Ttlv.Objects
import Ttlv.Structures
import qualified Ttlv.Enum as TEnum

ttlv :: T.TtlvTag -> TtlvData -> Ttlv
ttlv t d = Ttlv (T.Tag t) d

xx = ttlv T.Attribute (TtlvStructure [ ttlv T.AttributeName (TtlvString "x-hi")
                                     , ttlv T.AttributeIndex (TtlvInt 0)
                                     , ttlv T.AttributeValue (TtlvString "hello world") ])
yy = ttlv T.Attribute (TtlvStructure [ ttlv T.AttributeName (TtlvString "x-yo")
                                     , ttlv T.AttributeIndex (TtlvInt 1)
                                     , ttlv T.AttributeValue (TtlvString "sticker") ])

zz = ttlv T.Credential (TtlvStructure [ ttlv T.CredentialType (TtlvEnum $ TEnum.fromTtlvEnum TEnum.UsernameAndPassword)
                                      , ttlv T.CredentialValue (TtlvStructure [ ttlv T.Username (TtlvString "aaron")
                                                                              , ttlv T.Password (TtlvString "password") ])])


spec :: Spec
spec = do
  describe "Validator" $ do
    it "should sub-apply validators" $ do
      let x = apply T.AttributeName (stringEq "x-hi")
      runTtlvParser x xx `shouldBe` Right xx
    it "should allow chaining of validators" $ do
      let x = apply T.AttributeName (stringEq "x-hi")
      let y = apply T.AttributeValue ok
      runTtlvParser (x <+> y) xx `shouldBe` Right xx
    it "should allow chaining of validators (alternation)" $ do
      let x = apply T.AttributeName (stringEq "x-hi")
          y = apply T.AttributeName (stringEq "x-yo")
          p = x <|> y
      runTtlvParser p xx `shouldBe` Right xx
      runTtlvParser p yy `shouldBe` Right yy
    it "should fail bad matches" $ do
      let x = apply T.AttributeName (stringEq "abc")
      runTtlvParser x xx `shouldBe` Left ["mismatch in expected value"]
    it "should allow many (one-many)" $ do
      let xx' = ttlv T.Attribute (TtlvStructure [xx, yy])
      runTtlvParser (many1 T.Attribute attribute_) xx' `shouldBe` Right xx'
      runTtlvParser (many1 T.ArchiveDate attribute_) xx' `shouldBe` Left ["unable to find tag ArchiveDate"]
    it "should allow many (zero-many)" $ do
      let xx' = ttlv T.Attribute (TtlvStructure [xx, yy])
      runTtlvParser (many T.Attribute attribute_) xx' `shouldBe` Right xx'
      runTtlvParser (many T.ArchiveDate attribute_) xx' `shouldBe` Right xx'

    describe "Objects" $ do
      describe "Attribute" $ do
        it "valid" $ do
          runTtlvParser (attribute "x-hi" string) xx `shouldBe` Right xx
        it "unexpected attribute" $ do
          runTtlvParser (attribute "x-hi" tint) xx `shouldBe` Left ["failed combinator", "not an int"]
      describe "Credential" $ do
        it "valid" $ do
          runTtlvParser credential zz `shouldBe` Right zz
