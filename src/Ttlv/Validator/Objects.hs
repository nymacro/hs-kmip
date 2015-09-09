-- KMIP Objects
module Ttlv.Validator.Objects  where

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Structures

attribute :: String -> TtlvParser Ttlv -> TtlvParser Ttlv
attribute name vf = do
  tag T.Attribute
  apply    T.AttributeName (stringEq name)
  apply    T.AttributeValue vf
  optional T.AttributeIndex tint

attribute_ :: TtlvParser Ttlv
attribute_ = do
  tag T.Attribute
  apply    T.AttributeName ok
  apply    T.AttributeValue ok
  optional T.AttributeIndex tint

credential :: TtlvParser Ttlv
credential = do
  tag T.Credential
  apply T.CredentialType tenum
  apply T.CredentialValue credentialValue

credentialValue :: TtlvParser Ttlv
credentialValue = do
  apply T.Username string
  apply T.Password string

keyBlock :: TtlvParser Ttlv
keyBlock = do
  tag      T.KeyBlock
  apply    T.KeyFormatType tenum
  optional T.KeyCompressionType tenum
  apply    T.KeyValue keyValue
  optional T.CryptographicAlgorithm tenum  -- FIXME
  optional T.CryptographicLength tint   -- FIXME
  optional T.KeyWrappingData tstruct -- FIXME

keyValue :: TtlvParser Ttlv
keyValue = do
  apply    T.KeyMaterial keyMaterial
  many     T.Attribute   attribute_

keyWrappingData :: TtlvParser Ttlv
keyWrappingData = do
  --tag      T.WrappingData <+>
  apply    T.WrappingMethod tenum
  optional T.EncryptionKeyInformation encryptionKeyInfo
  optional T.MACSignatureKeyInformation macSignatureKeyInfo
  optional T.MACSignature tbytestring
  optional T.IVCounterNonce tbytestring

encryptionKeyInfo :: TtlvParser Ttlv
encryptionKeyInfo = do
  apply    T.UniqueIdentifier tstring
  optional T.CryptographicParameters tstruct

macSignatureKeyInfo :: TtlvParser Ttlv
macSignatureKeyInfo = do
  apply    T.UniqueIdentifier tstring
  optional T.CryptographicParameters tstruct


keyWrappingSpec :: TtlvParser Ttlv
keyWrappingSpec = do
  apply    T.WrappingMethod tenum
  optional T.EncryptionKeyInformation tstruct
  optional T.MACSignatureKeyInformation tstruct
  many     T.Attribute attribute_

keyMaterial :: TtlvParser Ttlv
keyMaterial = keyMaterialSymmetricKey <|>
              keyMaterialDSAPrivateKey <|>
              keyMaterialDSAPublicKey <|>
              keyMaterialRSAPrivateKey <|>
              keyMaterialRSAPublicKey

keyMaterialSymmetricKey :: TtlvParser Ttlv
keyMaterialSymmetricKey = apply T.Key tbytestring

keyMaterialDSAPrivateKey :: TtlvParser Ttlv
keyMaterialDSAPrivateKey = do
  apply T.P tbigint
  apply T.Q tbigint
  apply T.G tbigint
  apply T.X tbigint

keyMaterialDSAPublicKey :: TtlvParser Ttlv
keyMaterialDSAPublicKey = do
  apply T.P tbigint
  apply T.Q tbigint
  apply T.G tbigint
  apply T.Y tbigint

keyMaterialRSAPrivateKey :: TtlvParser Ttlv
keyMaterialRSAPrivateKey = do
  optional T.Modulus tbigint
  optional T.PrivateExponent tbigint
  optional T.PublicExponent tbigint
  optional T.P tbigint
  optional T.Q tbigint
  optional T.PrimeExponentP tbigint
  optional T.PrimeExponentQ tbigint
  optional T.CRTCoefficient tbigint

keyMaterialRSAPublicKey :: TtlvParser Ttlv
keyMaterialRSAPublicKey = do
  apply T.Modulus tbigint
  apply T.PublicExponent tbigint

-- TODO DH/ECDSA/ECDH/ECMQV key materials

templateAttribute :: TtlvParser Ttlv
templateAttribute = do
  optional T.Name tstruct
  optional T.Attribute attribute_


-- Managed Objects
certificate :: TtlvParser Ttlv
certificate = do
  tag   T.Certificate
  apply T.CertificateType tenum
  apply T.CertificateValue tbytestring

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
  apply T.SplitKeyParts tint
  apply T.KeyPartIdentifier tint
  apply T.SplitKeyThreshold tint
  apply T.SplitKeyMethod tenum
  apply T.PrimeFieldSize tbigint
  apply T.KeyBlock keyBlock

template :: TtlvParser Ttlv
template = do
  tag   T.Template
  many  T.Attribute attribute_


secretData :: TtlvParser Ttlv
secretData = do
  tag   T.SecretData
  apply T.SecretDataType tenum
  apply T.KeyBlock keyBlock

opaqueObject :: TtlvParser Ttlv
opaqueObject = do
  tag   T.OpaqueObject
  apply T.OpaqueDataType tenum
  apply T.OpaqueDataValue tbytestring

cryptoObject :: TtlvParser Ttlv
cryptoObject = apply T.Certificate certificate <|>
               apply T.SymmetricKey symmetricKey <|>
               apply T.PublicKey publicKey <|>
               apply T.PrivateKey privateKey <|>
               apply T.SplitKey splitKey <|>
               apply T.Template template <|>
               apply T.SecretData secretData <|>
               apply T.OpaqueObject opaqueObject
