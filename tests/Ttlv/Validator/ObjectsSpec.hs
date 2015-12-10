module Ttlv.Validator.ObjectsSpec where

import           Test.Hspec

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Enum                 as TEnum
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Objects
import           Ttlv.Validator.Structures
import           Ttlv.Validator.Types

ttlv :: T.TtlvTag -> TtlvData -> Ttlv
ttlv t = Ttlv (T.Tag t)

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
    describe "basic" $ do
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
    describe "monad" $ do
      it "should work correctly with monadic validator" $ do
        let p = do
              tStruct
              tag T.Attribute
              apply T.AttributeName (stringEq "x-hi")
        runTtlvParser p xx `shouldBe` Right xx
        let xx' = ttlv T.TemplateAttribute (TtlvStructure [ttlv T.Attribute (TtlvStructure [ ttlv T.AttributeName (TtlvString "x-hi")
                                                                                           , ttlv T.AttributeValue (TtlvString "hello") ] ) ] )
            p' = do
              tStruct
              apply T.Attribute $ do
                tStruct
                apply T.AttributeName  (stringEq "x-hi")
                apply T.AttributeValue (stringEq "hello")
        runTtlvParser p' xx' `shouldBe` Right xx'

      it "should allow extraction of data" $ do
        let xx' = ttlv T.TemplateAttribute (TtlvStructure [ttlv T.Attribute (TtlvStructure [ ttlv T.AttributeName (TtlvString "x-hi")
                                                                                           , ttlv T.AttributeValue (TtlvString "hello") ] ) ] )
            p = do
              tStruct
              apply T.Attribute $ do
                x <- get T.AttributeName
                if x == TtlvString "x-hi"
                then ok
                else nok "couldn't extract"
        runTtlvParser p xx' `shouldBe` Right xx'

      it "should fail if validation fails" $ do
        let p = do
              tInt
              tag T.Attribute
              apply T.AttributeName (stringEq "x-hi")
            p' = do
                  tStruct
                  tag T.Attribute
                  apply T.AttributeName (stringEq "y-nope")
        runTtlvParser p xx `shouldBe` Left ["failed combinator", "not an int"]
        runTtlvParser p' xx `shouldBe` Left ["mismatch in expected value"]

    describe "Objects" $ do
      describe "Attribute" $ do
        it "valid" $ do
          runTtlvParser (attribute "x-hi" string) xx `shouldBe` Right xx
        it "unexpected attribute" $ do
          runTtlvParser (attribute "x-hi" tInt) xx `shouldBe` Left ["failed combinator", "not an int"]
      describe "Credential" $ do
        it "valid" $ do
          runTtlvParser credential zz `shouldBe` Right zz

    describe "Extraction" $ do
      it "ok" $ do
        runTtlvParser ok xx `shouldBe` Right xx
      it "under" $ do
        runTtlvParser (under T.AttributeName) xx `shouldBe` Right (ttlv T.AttributeName $ TtlvString "x-hi")
