-- KMIP Client-to-Server operations
module Ttlv.Validator.Operations where

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Attributes
import           Ttlv.Validator.Objects
import           Ttlv.Validator.Structures

requestOperationFor :: Int -> Maybe (TtlvParser Ttlv)
requestOperationFor t = case t of
  1  -> Just createRequest
  2  -> Just createKeyPairRequest
  3  -> Just registerRequest
  4  -> Just reKeyRequest
  5  -> Just deriveKeyRequest
  6  -> Just certifyRequest
  7  -> Just reCertifyRequest
  8  -> Just locateRequest
  9  -> Just checkRequest
  10 -> Just getRequest
  11 -> Just getAttributeRequest
  12 -> Just getAttributeListRequest
  13 -> Just addAttributeRequest
  14 -> Just modifyAttributeRequest
  15 -> Just deleteAttributeRequest
  16 -> Just obtainLeaseRequest
  17 -> Just getUsageAllocationRequest
  18 -> Just activateRequest
  19 -> Just revokeRequest
  20 -> Just destroyRequest
  21 -> Just archiveRequest
  22 -> Just recoverRequest
  23 -> Just validateRequest
  24 -> Just queryRequest
  25 -> Just cancelRequest
  26 -> Just pollRequest
--  27 -> notifyRequest
--  28 -> putRequest
  _ -> Nothing

requestOperation = createRequest <|>
                   createKeyPairRequest <|>
                   registerRequest <|>
                   reKeyRequest <|>
                   deriveKeyRequest <|>
                   certifyRequest <|>
                   reCertifyRequest <|>
                   locateRequest <|>
                   checkRequest <|>
                   getRequest <|>
                   getAttributeRequest <|>
                   getAttributeListRequest <|>
                   addAttributeRequest <|>
                   modifyAttributeRequest <|>
                   deleteAttributeRequest <|>
                   obtainLeaseRequest <|>
                   getUsageAllocationRequest <|>
                   activateRequest <|>
                   revokeRequest <|>
                   destroyRequest <|>
                   archiveRequest <|>
                   recoverRequest <|>
                   validateRequest <|>
                   queryRequest <|>
                   cancelRequest <|>
                   pollRequest

responseOperationFor :: Int -> Maybe (TtlvParser Ttlv)
responseOperationFor t = case t of
  1  -> Just createResponse
  2  -> Just createKeyPairResponse
  3  -> Just registerResponse
  4  -> Just reKeyResponse
  5  -> Just deriveKeyResponse
  6  -> Just certifyResponse
  7  -> Just reCertifyResponse
  8  -> Just locateResponse
  9  -> Just checkResponse
  10 -> Just getResponse
  11 -> Just getAttributeResponse
  12 -> Just getAttributeListResponse
  13 -> Just addAttributeResponse
  14 -> Just modifyAttributeResponse
  15 -> Just deleteAttributeResponse
  16 -> Just obtainLeaseResponse
  17 -> Just getUsageAllocationResponse
  18 -> Just activateResponse
  19 -> Just revokeResponse
  20 -> Just destroyResponse
  21 -> Just archiveResponse
  22 -> Just recoverResponse
  23 -> Just validateResponse
  24 -> Just queryResponse
  25 -> Just cancelResponse
  26 -> Just pollResponse
  _ -> Nothing

responseOperation = createResponse <|>
                    createKeyPairResponse <|>
                    registerResponse <|>
                    reKeyResponse <|>
                    deriveKeyResponse <|>
                    certifyResponse <|>
                    reCertifyResponse <|>
                    locateResponse <|>
                    checkResponse <|>
                    getResponse <|>
                    getAttributeResponse <|>
                    getAttributeListResponse <|>
                    addAttributeResponse <|>
                    modifyAttributeResponse <|>
                    deleteAttributeResponse <|>
                    obtainLeaseResponse <|>
                    getUsageAllocationResponse <|>
                    activateResponse <|>
                    revokeResponse <|>
                    destroyResponse <|>
                    archiveResponse <|>
                    recoverResponse <|>
                    validateResponse <|>
                    queryResponse <|>
                    cancelResponse <|>
                    pollResponse

serverRequestOperation = serverNotifyRequest <|>
                         serverPutRequest

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

getAttributeRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  many     T.AttributeName tstring

getAttributeResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  many     T.Attribute attribute_

getAttributeListRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

getAttributeListResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  many     T.Attribute attribute_

addAttributeRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  apply    T.Attribute attribute_

addAttributeResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  apply    T.Attribute attribute_

modifyAttributeRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  apply    T.Attribute attribute_

modifyAttributeResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  apply    T.Attribute attribute_

deleteAttributeRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  apply    T.AttributeName tstring
  optional T.AttributeIndex tint

deleteAttributeResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  apply    T.Attribute attribute_

obtainLeaseRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

obtainLeaseResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier
  apply    T.LeaseTime tinterval
  apply    T.LastChangeDate tdatetime

getUsageAllocationRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  apply    T.UsageLimitsCount tint

getUsageAllocationResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

activateRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

activateResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

revokeRequest = do
  optional T.UniqueIdentifier uniqueIdentifier
  apply    T.RevocationReason revocationReason
  optional T.CompromiseOccurrenceDate tdatetime

revokeResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

destroyRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

destroyResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

archiveRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

archiveResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

recoverRequest = do
  optional T.UniqueIdentifier uniqueIdentifier

recoverResponse = do
  apply    T.UniqueIdentifier uniqueIdentifier

validateRequest = do
  many     T.Certificate certificate
  many     T.UniqueIdentifier uniqueIdentifier
  optional T.ValidityDate tdatetime

validateResponse = do
  apply    T.ValidityIndicator tenum

queryRequest = do
  apply    T.QueryFunction tenum

queryResponse = do
  many     T.Operation tenum
  many     T.ObjectType tenum
  optional T.VendorIdentification tstring
  optional T.ServerInformation tstring
  many     T.ApplicationNamespace applicationSpecificInfo

cancelRequest = do
  apply    T.AsynchronousCorrelationValue ok -- FIXME type

cancelResponse = do
  apply    T.AsynchronousCorrelationValue ok -- FIXME type
  apply    T.CancellationResult ok -- FIXME type

pollRequest = do
  apply    T.AsynchronousCorrelationValue ok -- FIXME type

pollResponse = do
  ok

-- Server -> Client Operations
serverNotifyRequest = do
  apply    T.UniqueIdentifier uniqueIdentifier
  many1    T.Attribute attribute_

serverPutRequest = do
  apply    T.UniqueIdentifier uniqueIdentifier
  apply    T.PutFunction tenum
  optional T.ReplacedUniqueIdentifier uniqueIdentifier
  cryptoObject
  many     T.Attribute attribute_


