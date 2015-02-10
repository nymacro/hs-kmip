-- KMIP Objects
module Ttlv.Objects  where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Structures
import qualified Ttlv.Enum as TEnum

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

cryptoObject = certificate <|>
               symmetricKey <|>
               publicKey <|>
               privateKey <|>
               splitKey <|>
               template <|>
               secretData <|>
               opaqueObject
               


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

