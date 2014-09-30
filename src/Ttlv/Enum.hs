module Ttlv.Enum where
import Ttlv.Tag
import Ttlv.Data

import Test.Hspec

class Enum a => TtlvEnumType a where
  toTtlvEnum :: Int -> a
  toTtlvEnum = toEnum . pred

  fromTtlvEnum :: a -> Int
  fromTtlvEnum = succ . fromEnum

  toTtlvEnumType :: a -> TtlvData
  toTtlvEnumType x = TtlvEnum $ fromTtlvEnum x

  fromTtlvEnumType :: TtlvData -> a
  fromTtlvEnumType (TtlvEnum x) = toTtlvEnum x
  fromTtlvEnumType _ = undefined

data CredentialType = UsernameAndPassword
                    deriving (Show, Eq, Enum)
instance TtlvEnumType CredentialType

data KeyCompressionType = ECPubKeyUncompressed
                        | ECPubKeyCompressedPrime
                        | ECPubKeyCompressedChar2
                        | ECPubKeyHybrid
                        deriving (Show, Eq, Enum)
instance TtlvEnumType KeyCompressionType

-- toKeyCompressionType :: Int -> KeyCompressionType
-- toKeyCompressionType = toEnum . pred
-- fromKeyCompressionType :: KeyCompressionType -> Int
-- fromKeyCompressionType = succ . fromEnum

data KeyFormatType = Raw
                   | Opaque
                   | PKCS1
                   | PKCS8
                   | X509
                   | ECPrivateKey
                   | SymmetricKey
                   | DSAPrivateKey
                   | DSAPublicKey
                   | RSAPrivateKey
                   | RSAPublicKey
                   | DHPrivateKey
                   | DHPublicKey
                   | ECDSAPrivateKey
                   | ECDSAPublicKey
                   | ECDHPrivateKey
                   | ECDHPublicKey
                   | ECMQVPrivateKey
                   | ECMQVPublicKey
                   deriving (Show, Eq, Enum)
instance TtlvEnumType KeyFormatType

-- toKeyFormatType :: Int -> KeyFormatType
-- toKeyFormatType = toEnum . pred
-- fromKeyFormatType :: KeyFormatType -> Int
-- fromKeyFormatType = succ . fromEnum


data WrappingMethod = Encrypt
                    | MACSign
                    | EncryptMACSign
                    | MACSignEncrypt
                    | TR31
                    deriving (Show, Eq, Enum)
instance TtlvEnumType WrappingMethod

-- toWrappingMethod :: Int -> WrappingMethod
-- toWrappingMethod = toEnum . pred
-- fromWrappingMethod :: WrappingMethod -> Int
-- fromWrappingMethod = succ . fromEnum

data RecommendedCurve = P192 
                      | K163 
                      | B163 
                      | P224 
                      | K233 
                      | B233 
                      | P256 
                      | K283 
                      | B283 
                      | P384 
                      | K409 
                      | B409 
                      | P521 
                      | K571 
                      | B571 
                      deriving (Show, Eq, Enum)
instance TtlvEnumType RecommendedCurve

-- toRecommendedCurve :: Int -> RecommendedCurve
-- toRecommendedCurve = toEnum . pred
-- fromRecommendedCurve :: RecommendedCurve -> Int
-- fromRecommendedCurve = succ . fromEnum

data CertificateType = CertX509
                     | CertPGP
                     deriving (Show, Eq, Enum)
instance TtlvEnumType CertificateType

-- toCertificateType :: Int -> CertificateType
-- toCertificateType = toEnum . pred
-- fromCertificateType :: CertificateType -> Int
-- fromCertificateType = succ . fromEnum


data SplitMethod = XOR
                 | PolynomialSharingGF
                 | PolynomialSharingPrimeField
                 deriving (Show, Eq, Enum)
instance TtlvEnumType SplitMethod

-- toSplitMethod :: Int -> SplitMethod
-- toSplitMethod = toEnum . pred
-- fromSplitMethod :: SplitMethod -> Int
-- fromSplitMethod = succ . fromEnum


data SecretDataType = Password
                    | Seed
                    deriving (Show, Eq, Enum)
instance TtlvEnumType SecretDataType

-- toSecretDataType :: Int -> SecretDataType
-- toSecretDataType = toEnum . pred
-- fromSecretDataType :: SecretDataType -> Int
-- fromSecretDataType = succ . fromEnum

-- data OpaqueDataType = undefined

data NameType = UninterpretedTextString
              | URI
              deriving (Show, Eq, Enum)
instance TtlvEnumType NameType

-- toNameType :: Int -> NameType
-- toNameType = toEnum . pred
-- fromNameType :: NameType -> Int
-- fromNameType = succ . fromEnum


data ObjectType = Certificate
                | SymmetricKeyType -- ++ Type
                | PublicKey
                | PrivateKey
                | SplitKey
                | Template
                | SecretData
                | OpaqueObject
                deriving (Show, Eq, Enum)
instance TtlvEnumType ObjectType

-- toObjectType :: Int -> ObjectType
-- toObjectType = toEnum . pred
-- fromObjectType :: ObjectType -> Int
-- fromObjectType = succ . fromEnum


data CryptoAlgorithm = DES
                     | DES3
                     | AES
                     | RSA
                     | DSA
                     | ECDSA
                     | HMACSHA1
                     | HMACSHA224
                     | HMACSHA256
                     | HMACSHA384
                     | HMACSHA512
                     | HMACMD5
                     | DH
                     | ECDH
                     | ECMQV
                     | Blowfish
                     | Camellia
                     | CAST5
                     | IDEA
                     | MARS
                     | RC2
                     | RC4
                     | RC5
                     | SKIPJACK
                     | Twofish
                     deriving (Show, Eq, Enum)
instance TtlvEnumType CryptoAlgorithm

-- toCryptoAlgorithm :: Int -> CryptoAlgorithm
-- toCryptoAlgorithm = toEnum . pred
-- fromCryptoAlgorithm :: CryptoAlgorithm -> Int
-- fromCryptoAlgorithm = succ . fromEnum


data BlockCipherMode = CBC 
                     | ECB 
                     | PCBC 
                     | CFB 
                     | OFB 
                     | CTR 
                     | CMAC 
                     | CCM 
                     | GCM 
                     | CBCMAC 
                     | XTS 
                     | AESKeyWrapPadding 
                     | NISTKeyWrap 
                     | X9102AESKW 
                     | X9102TDKW 
                     | X9102AKW1 
                     | X9102AKW2 
                     deriving (Show, Eq, Enum)
instance TtlvEnumType BlockCipherMode

-- toBlockCipherMode :: Int -> BlockCipherMode
-- toBlockCipherMode = toEnum . pred
-- fromBlockCipherMode :: BlockCipherMode -> Int
-- fromBlockCipherMode = succ . fromEnum


data PaddingMethod = None 
                   | OAEP 
                   | PKCS5 
                   | SSL3 
                   | Zeros 
                   | ANSIX923 
                   | ISO10126 
                   | PKCS1v15 
                   | X931 
                   | PSS
                   deriving (Show, Eq, Enum)
instance TtlvEnumType PaddingMethod

-- toPaddingMethod :: Int -> PaddingMethod
-- toPaddingMethod = toEnum . pred
-- fromPaddingMethod :: PaddingMethod -> Int
-- fromPaddingMethod = succ . fromEnum


test :: IO ()
test = do
  hspec $ do
    describe "Ttlv.Enum" $ do
      it "should convert between KeyCompressionType" $ do
        (toTtlvEnum 1 :: KeyCompressionType) `shouldBe` ECPubKeyUncompressed
        fromTtlvEnum ECPubKeyUncompressed `shouldBe` 1
        -- toKeyCompressionType 1 `shouldBe` ECPubKeyUncompressed
        -- fromKeyCompressionType ECPubKeyUncompressed `shouldBe` 1
