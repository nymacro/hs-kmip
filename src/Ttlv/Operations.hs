-- KMIP Client-to-Server operations
module Ttlv.Operations where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Structures
import Ttlv.Objects
import Ttlv.Attributes


createRequest = apply TtlvObjectType tenum <+>
                apply TtlvTemplateAttribute templateAttribute

createResponse = apply TtlvObjectType tenum <+>
                 apply TtlvUniqueIdentifier uniqueIdentifier <+>
                 optional TtlvTemplateAttribute templateAttribute

createKeyPairRequest = optional TtlvCommonTemplateAttribute templateAttribute <+>
                       optional TtlvPrivateKeyTemplateAttribute templateAttribute <+>
                       optional TtlvPublicKeyTemplateAttribute templateAttribute

createKeyPairResponse = apply TtlvPrivateKeyUniqueIdentifier uniqueIdentifier <+>
                        apply TtlvPublicKeyUniqueIdentifier uniqueIdentifier <+>
                        optional TtlvPrivateKeyTemplateAttribute templateAttribute <+>
                        optional TtlvPublicKeyTemplateAttribute templateAttribute

registerRequest = apply TtlvObjectType tenum <+>
                  apply TtlvTemplateAttribute templateAttribute <+>
                  cryptoObject

registerResponse = apply TtlvUniqueIdentifier uniqueIdentifier <+>
                   optional TtlvTemplateAttribute templateAttribute

reKeyRequest = optional TtlvUniqueIdentifier uniqueIdentifier <+>
               optional TtlvOffset tinterval <+>
               optional TtlvTemplateAttribute templateAttribute

reKeyResponse = apply TtlvUniqueIdentifier uniqueIdentifier <+>
                optional TtlvTemplateAttribute templateAttribute

deriveKeyRequest = nok "derive key not implemented"
deriveKeyResponse = nok "derive key not implemented"

certifyRequest  = optional TtlvUniqueIdentifier uniqueIdentifier <+>
                  apply TtlvCertificateRequestType tenum <+>
                  apply TtlvCertificateRequest tbytestring <+>
                  optional TtlvTemplateAttribute templateAttribute

certifyResponse = apply TtlvUniqueIdentifier uniqueIdentifier <+>
                  optional TtlvTemplateAttribute templateAttribute

reCertifyRequest = nok "re-certify not implemented"
reCertifyResponse = nok "re-certify not implemented"


locateRequest = optional TtlvMaximumItems tint <+>
                optional TtlvStorageStatusMask tint <+>
                many TtlvAttribute attribute_

locateResponse = many TtlvUniqueIdentifier uniqueIdentifier

checkRequest = optional TtlvUniqueIdentifier uniqueIdentifier <+>
               optional TtlvUsageLimitsCount tint <+> -- ???
               optional TtlvCryptographicUsageMask tint <+>
               optional TtlvLeaseTime tinterval -- ???

checkResponse = apply TtlvUniqueIdentifier uniqueIdentifier <+>
                optional TtlvUsageLimitsCount tint <+> -- ???
                optional TtlvCryptographicUsageMask tint <+>
                optional TtlvLeaseTime tinterval -- ???
                
getRequest = optional TtlvUniqueIdentifier uniqueIdentifier <+>
             optional TtlvKeyFormatType tenum <+>
             optional TtlvKeyCompressionType tenum <+>
             optional TtlvKeyWrappingSpecification tenum

getResponse = apply TtlvObjectType tenum <+>
              apply TtlvUniqueIdentifier uniqueIdentifier <+>
              cryptoObject

-- TODO getAttributes
