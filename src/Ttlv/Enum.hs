module Ttlv.Enum where
import Ttlv.Tag
import Ttlv.Data

import Test.Hspec

class TtlvEnumType a where
  toTtlvEnum :: TtlvEnumType a => a -> Ttlv

data CredentialType = UsernameAndPassword
                    deriving (Show, Eq, Enum)

data KeyCompressionType = ECPubKeyUncompressed
                        | ECPubKeyCompressedPrime
                        | ECPubKeyCompressedChar2
                        | ECPubKeyHybrid
                        deriving (Show, Eq, Enum)

toKeyCompressionType :: Int -> KeyCompressionType
toKeyCompressionType = toEnum
fromKeyCompressionType :: KeyCompressionType -> Int
fromKeyCompressionType = fromEnum

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
toKeyFormatType :: Int -> KeyFormatType
toKeyFormatType = toEnum
fromKeyFormatType :: KeyFormatType -> Int
fromKeyFormatType = fromEnum


data WrappingMethod = Encrypt
                    | MACSign
                    | EncryptMACSign
                    | MACSignEncrypt
                    | TR31
                    deriving (Show, Eq, Enum)

toWrappingMethod :: Int -> WrappingMethod
toWrappingMethod = toEnum
fromWrappingMethod :: WrappingMethod -> Int
fromWrappingMethod = fromEnum

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

toRecommendedCurve :: Int -> RecommendedCurve
toRecommendedCurve = toEnum
fromRecommendedCurve :: RecommendedCurve -> Int
fromRecommendedCurve = fromEnum

data CertificateType = CertX509
                     | CertPGP
                     deriving (Show, Eq, Enum)

toCertificateType :: Int -> CertificateType
toCertificateType = toEnum
fromCertificateType :: CertificateType -> Int
fromCertificateType = fromEnum


data SplitMethod = XOR
                 | PolynomialSharingGF
                 | PolynomialSharingPrimeField
                 deriving (Show, Eq, Enum)

toSplitMethod :: Int -> SplitMethod
toSplitMethod = toEnum
fromSplitMethod :: SplitMethod -> Int
fromSplitMethod = fromEnum


data SecretDataType = Password
                    | Seed
                    deriving (Show, Eq, Enum)

toSecretDataType :: Int -> SecretDataType
toSecretDataType = toEnum
fromSecretDataType :: SecretDataType -> Int
fromSecretDataType = fromEnum

-- data OpaqueDataType = undefined

data NameType = UninterpretedTextString
              | URI
              deriving (Show, Eq, Enum)

toNameType :: Int -> NameType
toNameType = toEnum
fromNameType :: NameType -> Int
fromNameType = fromEnum


data ObjectType = Certificate
                | SymmetricKeyType -- ++ Type
                | PublicKey
                | PrivateKey
                | SplitKey
                | Template
                | SecretData
                | OpaqueObject
                deriving (Show, Eq, Enum)

toObjectType :: Int -> ObjectType
toObjectType = toEnum
fromObjectType :: ObjectType -> Int
fromObjectType = fromEnum


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

toCryptoAlgorithm :: Int -> CryptoAlgorithm
toCryptoAlgorithm = toEnum
fromCryptoAlgorithm :: CryptoAlgorithm -> Int
fromCryptoAlgorithm = fromEnum


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

toBlockCipherMode :: Int -> BlockCipherMode
toBlockCipherMode = toEnum
fromBlockCipherMode :: BlockCipherMode -> Int
fromBlockCipherMode = fromEnum


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

toPaddingMethod :: Int -> PaddingMethod
toPaddingMethod = toEnum
fromPaddingMethod :: PaddingMethod -> Int
fromPaddingMethod = fromEnum


test :: IO ()
test = do
  hspec $ do
    describe "something" $ do
      it "shoudl do things" $ do
        True `shouldBe` True
