-- KMIP Client-to-Server operations
module Ttlv.Operations where

import qualified Ttlv.Tag as T
import Ttlv.Data
import Ttlv.Structures
import Ttlv.Objects
import Ttlv.Attributes

requestOperationFor t = case t of
  1 -> createRequest
  2 -> createKeyPairRequest
  3 -> registerRequest
  4 -> reKeyRequest
  5 -> deriveKeyRequest
  6 -> certifyRequest
  7 -> reCertifyRequest
  8 -> locateRequest
  9 -> checkRequest
  _ -> ok -- TODO rest of operations

requestOperation = createRequest <|>
                   createKeyPairRequest <|>
                   registerRequest <|>
                   reKeyRequest <|>
                   deriveKeyRequest <|>
                   certifyRequest <|>
                   reCertifyRequest <|>
                   locateRequest <|>
                   checkRequest

responseOperationFor t = case t of
  1 -> createResponse
  2 -> createKeyPairResponse
  3 -> registerResponse
  4 -> reKeyResponse
  5 -> deriveKeyResponse
  6 -> certifyResponse
  7 -> reCertifyResponse
  8 -> locateResponse
  9 -> checkResponse
  _ -> ok -- TODO rest of operations

responseOperation = createResponse <|>
                    createKeyPairResponse <|>
                    registerResponse <|>
                    reKeyResponse <|>
                    deriveKeyResponse <|>
                    certifyResponse <|>
                    reCertifyResponse <|>
                    locateResponse <|>
                    checkResponse

createRequest = apply T.ObjectType tenum <+>
                apply T.TemplateAttribute templateAttribute

createResponse = apply T.ObjectType tenum <+>
                 apply T.UniqueIdentifier uniqueIdentifier <+>
                 optional T.TemplateAttribute templateAttribute

createKeyPairRequest = optional T.CommonTemplateAttribute templateAttribute <+>
                       optional T.PrivateKeyTemplateAttribute templateAttribute <+>
                       optional T.PublicKeyTemplateAttribute templateAttribute

createKeyPairResponse = apply T.PrivateKeyUniqueIdentifier uniqueIdentifier <+>
                        apply T.PublicKeyUniqueIdentifier uniqueIdentifier <+>
                        optional T.PrivateKeyTemplateAttribute templateAttribute <+>
                        optional T.PublicKeyTemplateAttribute templateAttribute

registerRequest = apply T.ObjectType tenum <+>
                  apply T.TemplateAttribute templateAttribute <+>
                  cryptoObject

registerResponse = apply T.UniqueIdentifier uniqueIdentifier <+>
                   optional T.TemplateAttribute templateAttribute

reKeyRequest = optional T.UniqueIdentifier uniqueIdentifier <+>
               optional T.Offset tinterval <+>
               optional T.TemplateAttribute templateAttribute

reKeyResponse = apply T.UniqueIdentifier uniqueIdentifier <+>
                optional T.TemplateAttribute templateAttribute

deriveKeyRequest = nok "derive key not implemented"
deriveKeyResponse = nok "derive key not implemented"

certifyRequest  = optional T.UniqueIdentifier uniqueIdentifier <+>
                  apply T.CertificateRequestType tenum <+>
                  apply T.CertificateRequest tbytestring <+>
                  optional T.TemplateAttribute templateAttribute

certifyResponse = apply T.UniqueIdentifier uniqueIdentifier <+>
                  optional T.TemplateAttribute templateAttribute

reCertifyRequest = nok "re-certify not implemented"
reCertifyResponse = nok "re-certify not implemented"


locateRequest = optional T.MaximumItems tint <+>
                optional T.StorageStatusMask tint <+>
                many T.Attribute attribute_

locateResponse = many T.UniqueIdentifier uniqueIdentifier

checkRequest = optional T.UniqueIdentifier uniqueIdentifier <+>
               optional T.UsageLimitsCount tint <+> -- ???
               optional T.CryptographicUsageMask tint <+>
               optional T.LeaseTime tinterval -- ???

checkResponse = apply T.UniqueIdentifier uniqueIdentifier <+>
                optional T.UsageLimitsCount tint <+> -- ???
                optional T.CryptographicUsageMask tint <+>
                optional T.LeaseTime tinterval -- ???
                
getRequest = optional T.UniqueIdentifier uniqueIdentifier <+>
             optional T.KeyFormatType tenum <+>
             optional T.KeyCompressionType tenum <+>
             optional T.KeyWrappingSpecification tenum

getResponse = apply T.ObjectType tenum <+>
              apply T.UniqueIdentifier uniqueIdentifier <+>
              cryptoObject

-- TODO getAttributes
