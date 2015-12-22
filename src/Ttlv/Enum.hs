module Ttlv.Enum ( TtlvEnumType(..)
                 , CredentialType(..)
                 , KeyCompressionType(..)
                 , KeyFormatType(..)
                 , WrappingMethod(..)
                 , RecommendedCurve(..)
                 , CertificateType(..)
                 , SplitKeyMethod(..)
                 , SecretDataType(..)
                 , NameType(..)
                 , ObjectType(..)
                 , CryptoAlgorithm(..)
                 , BlockCipherMode(..)
                 , PaddingMethod(..)
                 , Operation(..) ) where

import           Ttlv.Data
import qualified Ttlv.Tag  as T
import           Data.Word

class Enum a => TtlvEnumType a where
  toTtlvEnum :: Word32 -> a
  toTtlvEnum = toEnum . pred . fromIntegral

  fromTtlvEnum :: a -> Word32
  fromTtlvEnum = fromIntegral . succ . fromEnum

  toTtlvEnumType :: a -> TtlvData
  toTtlvEnumType x = TtlvEnum $ fromIntegral $ fromTtlvEnum x

  fromTtlvEnumType :: TtlvData -> Maybe a
  fromTtlvEnumType (TtlvEnum x) = Just $ toTtlvEnum $ fromIntegral x
  fromTtlvEnumType _ = Nothing

  ttlvEnumTag :: a -> T.TtlvTag

data CredentialType = UsernameAndPassword
                    deriving (Show, Eq, Enum)
instance TtlvEnumType CredentialType where
  ttlvEnumTag _ = T.CredentialType

data KeyCompressionType = ECPubKeyUncompressed
                        | ECPubKeyCompressedPrime
                        | ECPubKeyCompressedChar2
                        | ECPubKeyHybrid
                        deriving (Show, Eq, Enum)
instance TtlvEnumType KeyCompressionType where
  ttlvEnumTag _ = T.KeyCompressionType


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
instance TtlvEnumType KeyFormatType where
  ttlvEnumTag _ = T.KeyFormatType


data WrappingMethod = Encrypt
                    | MACSign
                    | EncryptMACSign
                    | MACSignEncrypt
                    | TR31
                    deriving (Show, Eq, Enum)
instance TtlvEnumType WrappingMethod where
  ttlvEnumTag _ = T.WrappingMethod


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
instance TtlvEnumType RecommendedCurve where
  ttlvEnumTag _ = T.RecommendedCurve


data CertificateType = CertX509
                     | CertPGP
                     deriving (Show, Eq, Enum)
instance TtlvEnumType CertificateType where
  ttlvEnumTag _ = T.CertificateType


data SplitKeyMethod = XOR
                 | PolynomialSharingGF
                 | PolynomialSharingPrimeField
                 deriving (Show, Eq, Enum)
instance TtlvEnumType SplitKeyMethod where
  ttlvEnumTag _ = T.SplitKeyMethod


data SecretDataType = Password
                    | Seed
                    deriving (Show, Eq, Enum)
instance TtlvEnumType SecretDataType where
  ttlvEnumTag _ = T.SecretDataType


data NameType = UninterpretedTextString
              | URI
              deriving (Show, Eq, Enum)
instance TtlvEnumType NameType where
  ttlvEnumTag _ = T.NameType


data ObjectType = Certificate
                | SymmetricKeyType -- ++ Type
                | PublicKey
                | PrivateKey
                | SplitKey
                | Template
                | SecretData
                | OpaqueObject
                deriving (Show, Eq, Enum)
instance TtlvEnumType ObjectType where
  ttlvEnumTag _ = T.ObjectType


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
instance TtlvEnumType CryptoAlgorithm where
  ttlvEnumTag _ = T.CryptographicAlgorithm


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
instance TtlvEnumType BlockCipherMode where
  ttlvEnumTag _ = T.BlockCipherMode


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
instance TtlvEnumType PaddingMethod where
  ttlvEnumTag _ = T.PaddingMethod

data Operation = Create
               | CreateKeyPair
               | Register
               | ReKey
               deriving (Show, Eq, Enum)
instance TtlvEnumType Operation where
  ttlvEnumTag _ = T.Operation

-- test :: IO ()
-- test = do
--   hspec $ do
--     describe "Ttlv.Enum" $ do
--       it "should convert between KeyCompressionType" $ do
--         (toTtlvEnum 1 :: KeyCompressionType) `shouldBe` ECPubKeyUncompressed
--         fromTtlvEnum ECPubKeyUncompressed `shouldBe` 1
