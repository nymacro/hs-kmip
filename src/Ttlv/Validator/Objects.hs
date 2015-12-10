-- KMIP Objects
module Ttlv.Validator.Objects ( attribute
                              , attribute_
                              , credential
                              , credentialValue
                              , keyBlock
                              , keyValue
                              , keyWrappingData
                              , encryptionKeyInfo
                              , macSignatureKeyInfo
                              , keyWrappingSpec
                              , keyMaterial
                              , keyMaterialSymmetricKey
                              , keyMaterialDSAPrivateKey
                              , keyMaterialDSAPublicKey
                              , keyMaterialRSAPrivateKey
                              , keyMaterialRSAPublicKey
                              , templateAttribute
                              , certificate
                              , symmetricKey
                              , publicKey
                              , privateKey
                              , splitKey
                              , template
                              , secretData
                              , opaqueObject
                              , cryptoObject
                              ) where

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Structures
import           Ttlv.Validator.Types

attribute :: String -> TtlvParser Ttlv -> TtlvParser Ttlv
attribute name vf = do
  tag T.Attribute
  apply    T.AttributeName (stringEq name)
  apply    T.AttributeValue vf
  optional T.AttributeIndex tInt

attribute_ :: TtlvParser Ttlv
attribute_ = do
  tag T.Attribute
  apply    T.AttributeName ok
  apply    T.AttributeValue ok
  optional T.AttributeIndex tInt

credential :: TtlvParser Ttlv
credential = do
  tag T.Credential
  apply T.CredentialType tEnum
  apply T.CredentialValue credentialValue

credentialValue :: TtlvParser Ttlv
credentialValue = do
  apply T.Username string
  apply T.Password string

keyBlock :: TtlvParser Ttlv
keyBlock = do
  tag      T.KeyBlock
  apply    T.KeyFormatType tEnum
  optional T.KeyCompressionType tEnum
  apply    T.KeyValue keyValue
  optional T.CryptographicAlgorithm tEnum  -- FIXME
  optional T.CryptographicLength tInt   -- FIXME
  optional T.KeyWrappingData tStruct -- FIXME

keyValue :: TtlvParser Ttlv
keyValue = do
  apply    T.KeyMaterial keyMaterial
  many     T.Attribute   attribute_

keyWrappingData :: TtlvParser Ttlv
keyWrappingData = do
  --tag      T.WrappingData <+>
  apply    T.WrappingMethod tEnum
  optional T.EncryptionKeyInformation encryptionKeyInfo
  optional T.MACSignatureKeyInformation macSignatureKeyInfo
  optional T.MACSignature tByteString
  optional T.IVCounterNonce tByteString

encryptionKeyInfo :: TtlvParser Ttlv
encryptionKeyInfo = do
  apply    T.UniqueIdentifier tString
  optional T.CryptographicParameters tStruct

macSignatureKeyInfo :: TtlvParser Ttlv
macSignatureKeyInfo = do
  apply    T.UniqueIdentifier tString
  optional T.CryptographicParameters tStruct


keyWrappingSpec :: TtlvParser Ttlv
keyWrappingSpec = do
  apply    T.WrappingMethod tEnum
  optional T.EncryptionKeyInformation tStruct
  optional T.MACSignatureKeyInformation tStruct
  many     T.Attribute attribute_

keyMaterial :: TtlvParser Ttlv
keyMaterial = keyMaterialSymmetricKey <|>
              keyMaterialDSAPrivateKey <|>
              keyMaterialDSAPublicKey <|>
              keyMaterialRSAPrivateKey <|>
              keyMaterialRSAPublicKey

keyMaterialSymmetricKey :: TtlvParser Ttlv
keyMaterialSymmetricKey = apply T.Key tByteString

keyMaterialDSAPrivateKey :: TtlvParser Ttlv
keyMaterialDSAPrivateKey = do
  apply T.P tBigInt
  apply T.Q tBigInt
  apply T.G tBigInt
  apply T.X tBigInt

keyMaterialDSAPublicKey :: TtlvParser Ttlv
keyMaterialDSAPublicKey = do
  apply T.P tBigInt
  apply T.Q tBigInt
  apply T.G tBigInt
  apply T.Y tBigInt

keyMaterialRSAPrivateKey :: TtlvParser Ttlv
keyMaterialRSAPrivateKey = do
  optional T.Modulus tBigInt
  optional T.PrivateExponent tBigInt
  optional T.PublicExponent tBigInt
  optional T.P tBigInt
  optional T.Q tBigInt
  optional T.PrimeExponentP tBigInt
  optional T.PrimeExponentQ tBigInt
  optional T.CRTCoefficient tBigInt

keyMaterialRSAPublicKey :: TtlvParser Ttlv
keyMaterialRSAPublicKey = do
  apply T.Modulus tBigInt
  apply T.PublicExponent tBigInt

-- TODO DH/ECDSA/ECDH/ECMQV key materials

templateAttribute :: TtlvParser Ttlv
templateAttribute = do
  optional T.Name tStruct
  optional T.Attribute attribute_


-- Managed Objects
certificate :: TtlvParser Ttlv
certificate = do
  tag   T.Certificate
  apply T.CertificateType tEnum
  apply T.CertificateValue tByteString

-- TODO fix key block expectations
symmetricKey :: TtlvParser Ttlv
symmetricKey = do
  tag   T.SymmetricKey
  apply T.KeyBlock keyBlock

publicKey :: TtlvParser Ttlv
publicKey = do
  tag   T.PublicKey
  apply T.KeyBlock keyBlock

privateKey :: TtlvParser Ttlv
privateKey = do
  tag   T.PrivateKey
  apply T.KeyBlock keyBlock

splitKey :: TtlvParser Ttlv
splitKey = do
  tag   T.SplitKey
  apply T.SplitKeyParts tInt
  apply T.KeyPartIdentifier tInt
  apply T.SplitKeyThreshold tInt
  apply T.SplitKeyMethod tEnum
  apply T.PrimeFieldSize tBigInt
  apply T.KeyBlock keyBlock

template :: TtlvParser Ttlv
template = do
  tag   T.Template
  many  T.Attribute attribute_


secretData :: TtlvParser Ttlv
secretData = do
  tag   T.SecretData
  apply T.SecretDataType tEnum
  apply T.KeyBlock keyBlock

opaqueObject :: TtlvParser Ttlv
opaqueObject = do
  tag   T.OpaqueObject
  apply T.OpaqueDataType tEnum
  apply T.OpaqueDataValue tByteString

cryptoObject :: TtlvParser Ttlv
cryptoObject = apply T.Certificate certificate <|>
               apply T.SymmetricKey symmetricKey <|>
               apply T.PublicKey publicKey <|>
               apply T.PrivateKey privateKey <|>
               apply T.SplitKey splitKey <|>
               apply T.Template template <|>
               apply T.SecretData secretData <|>
               apply T.OpaqueObject opaqueObject
