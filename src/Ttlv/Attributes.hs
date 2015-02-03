module Ttlv.Attributes where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Structures

uniqueIdentifier :: TtlvParser Ttlv
uniqueIdentifier = tag TtlvUniqueIdentifier <+>
                   tstring

name :: TtlvParser Ttlv
name = tag TtlvName <+>
       apply TtlvNameValue tstring <+>
       apply TtlvNameType tenum

objectType :: TtlvParser Ttlv
objectType = tag TtlvObjectType <+>
             tenum

cryptographicAlgorithm :: TtlvParser Ttlv
cryptographicAlgorithm = tag TtlvCryptographicAlgorithm <+>
                         tenum

cryptographicLength :: TtlvParser Ttlv
cryptographicLength = tag TtlvCryptographicLength <+>
                      tint

cryptographicParameters = tag TtlvCryptographicParameters <+>
                          optional TtlvBlockCipherMode tenum <+>
                          optional TtlvPaddingMethod tenum <+>
                          optional TtlvHashingAlgorithm tenum <+>
                          optional TtlvKeyRoleType tenum

cryptographicDomainParameters = tag TtlvCryptographicDomainParameters <+>
                                optional TtlvQlength tint <+>
                                optional TtlvRecommendedCurve tenum

certificateType = tag TtlvCertificateType <+>
                  tenum

certificateIdentifier = tag      TtlvCertificateIdentifier <+>
                        apply    TtlvIssuer tstring <+>
                        optional TtlvSerialNumber tstring

certificateSubject = tag   TtlvCertificateSubject <+>
                     apply TtlvCertificateSubjectDistinguishedName tstring <+>
                     many  TtlvCertificateSubjectAlternativeName tstring

certificateIssuer = tag   TtlvCertificateIssuer <+>
                    apply TtlvCertificateIssuerDistinguishedName tstring <+>
                    many  TtlvCertificateIssuerAlternativeName tstring

digest = tag TtlvDigest <+>
         apply TtlvHashingAlgorithm tenum <+>
         apply TtlvDigestValue tbytestring

operationPolicyName = tag TtlvOperationPolicyName <+>
                      tstring

cryptographicUsageMask = tag TtlvCryptographicUsageMask <+>
                         tint

leaseTime = tag TtlvLeaseTime <+>
            tinterval

usageLimits = tag TtlvUsageLimits <+>
              apply TtlvUsageLimitsTotal tlong <+>
              apply TtlvUsageLimitsCount tlong <+>
              apply TtlvUsageLimitsUnit tlong

state = tag TtlvState <+>
        tenum

initialDate = tag TtlvInitialDate <+>
              tdatetime

activationDate = tag TtlvActivationDate <+>
                 tdatetime

processStartdate = tag TtlvProcessStartDate <+>
                   tdatetime

protectStopDate = tag TtlvProcessStartDate <+>
                  tdatetime

deactivationDate = tag TtlvDeactivationDate <+>
                   tdatetime

destroyDate = tag TtlvDestroyDate <+>
              tdatetime

compromiseOccurrenceDate = tag TtlvCompromiseOccurrenceDate <+>
                           tdatetime

compromiseDate = tag TtlvCompromiseDate <+>
                 tdatetime

revocationReason = tag TtlvRevocationReason <+>
                   apply TtlvRevocationReasonCode tenum <+>
                   optional TtlvRevocationMessage tstring

archiveDate = tag TtlvArchiveDate <+>
              tdatetime

-- TODO
-- objectGroup = _
