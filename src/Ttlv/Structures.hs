{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Ttlv.Structures where

import Ttlv.Tag
import Ttlv.Data
import Control.Lens
import qualified Data.List as L
import qualified Ttlv.Enum as TEnum

import Test.Hspec

newtype TtlvParser a = TtlvParser { runTtlvParser :: (a -> Either [String] a) }

-- instance Monad TtlvParser where
--   return  = TtlvParser
--   -- m a -> (a -> m b) -> m b
--   x >>= y = x <+> (return . y)

-- | Ttlv parser combinator
infixl 9 <+>
(<+>) :: TtlvParser Ttlv -> TtlvParser Ttlv -> TtlvParser Ttlv
x <+> y = TtlvParser $ \t ->
  case runTtlvParser x t of
    Right t' -> runTtlvParser y t'
    Left e -> Left $ "failed combinator" : e

-- | run one parser or the other
either :: TtlvParser a -> TtlvParser a -> TtlvParser a
either x y = TtlvParser $ \t -> case runTtlvParser x t of
  x'@(Right _) -> x'
  Left _ -> case runTtlvParser y t of
    y'@(Right _) -> y'
    Left e -> Left $ "neither matched" : e

(<|>) :: TtlvParser a -> TtlvParser a -> TtlvParser a
(<|>) = Ttlv.Structures.either


-- | on: Ttlv
-- | look at Ttlv and see if it matches; do not remove matched data
check :: String -> (Ttlv -> Bool) -> TtlvParser Ttlv
check msg f = TtlvParser $ \t -> if f t
                                 then Right t
                                 else Left ["failed check: " ++ msg]

-- | take struct `t` and run parser `p` on it if exists, removing
-- | the struct if it parses without error; otherwise return error
struct :: TtlvParser Ttlv -> Ttlv -> TtlvParser Ttlv
struct p t = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == length s
                then runTtlvParser p (head s)
                else Left ["couldn't find structure"]
    Nothing -> Left ["not a structure"]

-- | check if struct contains a specific tag
struct1 :: (Ttlv -> Bool) -> TtlvParser Ttlv
struct1 fn = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    (Just s) -> if 1 == (length $ filter fn s)
                then Right t
                else Left ["not enough/too many"]
    Nothing -> Left ["not a structure"]

-- | on: Structure
-- | return: The Ttlv for `tag`
-- | find a tag from structure
find :: TtlvTag -> TtlvParser Ttlv
find tag = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    Nothing -> Left ["not a structure"]
    Just s -> case filter (\t' -> getTtlvTag t' == tag) s of
      [] -> Left ["unable to find tag " ++ show tag]
      xs -> if length xs /= 1
            then Left ["too many/too few"]
            else Right $ head xs

-- | on: Ttlv
-- | zero-many
many :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
many tag v = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    Nothing -> Left ["not a structure"]
    Just s -> case filter (\t' -> getTtlvTag t' == tag) s of
      [] -> Right t
      xs -> if all (\x -> case x of
                       Right t -> True
                       Left _ -> False) $ map (runTtlvParser v) xs
            then Right t
            else Left ["not everything matched"]

-- | on: Ttlv
-- | one-many
many1 :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
many1 tag v = TtlvParser $ \t ->
  case (getTtlvData t) ^? _TtlvStructure of
    Nothing -> Left ["not a structure"]
    Just s -> case filter (\t' -> getTtlvTag t' == tag) s of
      [] -> Left ["unable to find tag " ++ show tag]
      xs -> if all (\x -> case x of
                       Right t -> True
                       Left _ -> False) $ map (runTtlvParser v) xs
            then Right t
            else Left ["not everything matched"]

-- | on: Ttlv
-- | check current Ttlv tag
tag :: TtlvTag -> TtlvParser Ttlv
tag tag' = TtlvParser $ \t -> if getTtlvTag t == tag'
                              then Right t
                              else Left ["current tag not " ++ show tag']

-- | on: Ttlv
-- | check Ttlv data type
string :: TtlvParser Ttlv
string = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  otherwise -> Left ["not a string"]

-- | on: Ttlv
-- | check Ttlv data type and value
stringEq :: String -> TtlvParser Ttlv
stringEq s = TtlvParser $ \t -> case getTtlvData t of
  TtlvString x -> if x == s
                  then Right t
                  else Left ["mismatch in expected value"]
  otherwise -> Left ["not a string"]

-- | on: Ttlv
-- | check Ttlv data type
tstruct :: TtlvParser Ttlv
tstruct = TtlvParser $ \t -> case getTtlvData t of
  TtlvStructure _ -> Right t
  otherwise -> Left ["not a structure"]

-- | on: Ttlv
-- | check Ttlv data type
tenum :: TtlvParser Ttlv
tenum = TtlvParser $ \t -> case getTtlvData t of
  TtlvEnum e -> Right t
  otherwise  -> Left ["not an enum"]

tbytestring :: TtlvParser Ttlv
tbytestring = TtlvParser $ \t -> case getTtlvData t of
  TtlvByteString e -> Right t
  otherwise  -> Left ["not a byte-string"]

tstring :: TtlvParser Ttlv
tstring = TtlvParser $ \t -> case getTtlvData t of
  TtlvString e -> Right t
  otherwise  -> Left ["not a string"]

tbigint :: TtlvParser Ttlv
tbigint = TtlvParser $ \t -> case getTtlvData t of
  TtlvBigInt e -> Right t
  otherwise  -> Left ["not a big int"]

tint :: TtlvParser Ttlv
tint = TtlvParser $ \t -> case getTtlvData t of
  TtlvInt e -> Right t
  otherwise  -> Left ["not an int"]

tlong :: TtlvParser Ttlv
tlong = TtlvParser $ \t -> case getTtlvData t of
  TtlvLongInt e -> Right t
  otherwise  -> Left ["not an int"]

tinterval :: TtlvParser Ttlv
tinterval = TtlvParser $ \t -> case getTtlvData t of
  TtlvInterval e -> Right t
  otherwise  -> Left ["not an int"]

tdatetime :: TtlvParser Ttlv
tdatetime = TtlvParser $ \t -> case getTtlvData t of
  TtlvDateTime e -> Right t
  otherwise  -> Left ["not an int"]

-- | on: Structure
-- | run `parser` on `tag`, returning original Ttlv
apply :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
apply tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left y  -> Left y

-- | on: Structure
-- | like apply, but will run `parser` only if tag exists. Will pass
-- | by default.
optional :: TtlvTag -> TtlvParser Ttlv -> TtlvParser Ttlv
optional tag parser = TtlvParser $ \t ->
  case runTtlvParser (find tag) t of
    Right x -> case runTtlvParser parser x of
      Right _ -> Right t
      Left y' -> Left y'
    Left _  -> Right t


-- | on: *
-- | identity for TtlvParser
ok :: TtlvParser Ttlv
ok = TtlvParser $ \t -> Right t

-- Objects

attribute :: String -> TtlvParser Ttlv -> TtlvParser Ttlv
attribute name vf = tag TtlvAttribute <+>
                    apply    TtlvAttributeName (stringEq name) <+>
                    apply    TtlvAttributeValue vf <+>
                    optional TtlvAttributeIndex tint

attribute_ :: TtlvParser Ttlv
attribute_ = tag TtlvAttribute <+>
             apply    TtlvAttributeName ok <+>
             apply    TtlvAttributeValue ok <+>
             optional TtlvAttributeIndex tint

credential :: TtlvParser Ttlv
credential = tag TtlvCredential <+>
             apply TtlvCredentialType tenum <+>
             apply TtlvCredentialValue credentialValue

credentialValue :: TtlvParser Ttlv
credentialValue = apply TtlvUsername string <+> apply TtlvPassword string

keyBlock :: TtlvParser Ttlv
keyBlock = tag      TtlvKeyBlock <+>
           apply    TtlvKeyFormatType tenum <+>
           optional TtlvKeyCompressionType tenum <+>
           apply    TtlvKeyValue keyValue <+>
           optional TtlvCryptographicAlgorithm tenum <+> -- FIXME
           optional TtlvCryptographicLength tint <+>  -- FIXME
           optional TtlvKeyWrappingData tstruct -- FIXME
           
keyValue :: TtlvParser Ttlv
keyValue = apply    TtlvKeyMaterial keyMaterial <+>
           many     TtlvAttribute   attribute_

keyWrappingData :: TtlvParser Ttlv
keyWrappingData = --tag      TtlvWrappingData <+>
                  apply    TtlvWrappingMethod tenum <+>
                  optional TtlvEncryptionKeyInformation encryptionKeyInfo <+>
                  optional TtlvMACSignatureKeyInformation macSignatureKeyInfo <+>
                  optional TtlvMACSignature tbytestring <+>
                  optional TtlvIVCounterNonce tbytestring

encryptionKeyInfo :: TtlvParser Ttlv
encryptionKeyInfo = apply    TtlvUniqueIdentifier tstring <+>
                    optional TtlvCryptographicParameters tstruct

macSignatureKeyInfo :: TtlvParser Ttlv
macSignatureKeyInfo = apply    TtlvUniqueIdentifier tstring <+>
                      optional TtlvCryptographicParameters tstruct


keyWrappingSpec :: TtlvParser Ttlv
keyWrappingSpec = apply    TtlvWrappingMethod tenum <+>
                  optional TtlvEncryptionKeyInformation tstruct <+>
                  optional TtlvMACSignatureKeyInformation tstruct <+>
                  many     TtlvAttribute attribute_

keyMaterial :: TtlvParser Ttlv
keyMaterial = keyMaterialSymmetricKey <|>
              keyMaterialDSAPrivateKey <|>
              keyMaterialDSAPublicKey <|>
              keyMaterialRSAPrivateKey <|>
              keyMaterialRSAPublicKey

keyMaterialSymmetricKey :: TtlvParser Ttlv
keyMaterialSymmetricKey = apply TtlvKey tbytestring

keyMaterialDSAPrivateKey :: TtlvParser Ttlv
keyMaterialDSAPrivateKey = apply TtlvP tbigint <+>
                           apply TtlvQ tbigint <+>
                           apply TtlvG tbigint <+>
                           apply TtlvX tbigint

keyMaterialDSAPublicKey :: TtlvParser Ttlv
keyMaterialDSAPublicKey = apply TtlvP tbigint <+>
                          apply TtlvQ tbigint <+>
                          apply TtlvG tbigint <+>
                          apply TtlvY tbigint

keyMaterialRSAPrivateKey :: TtlvParser Ttlv
keyMaterialRSAPrivateKey = optional TtlvModulus tbigint <+>
                           optional TtlvPrivateExponent tbigint <+>
                           optional TtlvPublicExponent tbigint <+>
                           optional TtlvP tbigint <+>
                           optional TtlvQ tbigint <+>
                           optional TtlvPrimeExponentP tbigint <+>
                           optional TtlvPrimeExponentQ tbigint <+>
                           optional TtlvCRTCoefficient tbigint

keyMaterialRSAPublicKey :: TtlvParser Ttlv
keyMaterialRSAPublicKey = apply TtlvModulus tbigint <+>
                          apply TtlvPublicExponent tbigint

-- TODO DH/ECDSA/ECDH/ECMQV key materials

templateAttribute :: TtlvParser Ttlv
templateAttribute = optional TtlvName tstruct <+>
                    optional TtlvAttribute attribute_


-- Managed Objects
certificate :: TtlvParser Ttlv
certificate = tag   TtlvCertificate <+>
              apply TtlvCertificateType tenum <+>
              apply TtlvCertificateValue tbytestring

-- TODO fix key block expectations
symmetricKey :: TtlvParser Ttlv
symmetricKey = tag   TtlvSymmetricKey <+>
               apply TtlvKeyBlock keyBlock

publicKey :: TtlvParser Ttlv
publicKey = tag   TtlvPublicKey <+>
            apply TtlvKeyBlock keyBlock

privateKey :: TtlvParser Ttlv
privateKey = tag   TtlvPrivateKey <+>
             apply TtlvKeyBlock keyBlock

splitKey :: TtlvParser Ttlv
splitKey = tag   TtlvSplitKey <+>
           apply TtlvSplitKeyParts tint <+>
           apply TtlvKeyPartIdentifier tint <+>
           apply TtlvSplitKeyThreshold tint <+>
           apply TtlvSplitKeyMethod tenum <+>
           apply TtlvPrimeFieldSize tbigint <+>
           apply TtlvKeyBlock keyBlock

template :: TtlvParser Ttlv
template = tag   TtlvTemplate <+>
           many  TtlvAttribute attribute_


secretData :: TtlvParser Ttlv
secretData = tag   TtlvSecretData <+>
             apply TtlvSecretDataType tenum <+>
             apply TtlvKeyBlock keyBlock

opaqueObject :: TtlvParser Ttlv
opaqueObject = tag   TtlvOpaqueObject <+>
               apply TtlvOpaqueDataType tenum <+>
               apply TtlvOpaqueDataValue tbytestring



-- Testing
xx = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-hi")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 0)
                                        , Ttlv TtlvAttributeValue (TtlvString "hello world") ]))
yy = (Ttlv TtlvAttribute (TtlvStructure [ Ttlv TtlvAttributeName (TtlvString "x-yo")
                                        , Ttlv TtlvAttributeIndex (TtlvInt 1)
                                        , Ttlv TtlvAttributeValue (TtlvString "sticker") ]))

zz = Ttlv TtlvCredential (TtlvStructure [ Ttlv TtlvCredentialType (TtlvEnum $ TEnum.fromTtlvEnum TEnum.UsernameAndPassword)
                                        , Ttlv TtlvCredentialValue (TtlvStructure [ Ttlv TtlvUsername (TtlvString "aaron")
                                                                                  , Ttlv TtlvPassword (TtlvString "password") ])])

test :: IO ()
test = hspec $ do
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
