-- KMIP Attribute

module Ttlv.Validator.Attributes where

import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Structures

uniqueIdentifier :: TtlvParser Ttlv
uniqueIdentifier = tag T.UniqueIdentifier <+>
                   tstring

name :: TtlvParser Ttlv
name = tag T.Name <+>
       apply T.NameValue tstring <+>
       apply T.NameType tenum

objectType :: TtlvParser Ttlv
objectType = tag T.ObjectType <+>
             tenum

cryptographicAlgorithm :: TtlvParser Ttlv
cryptographicAlgorithm = tag T.CryptographicAlgorithm <+>
                         tenum

cryptographicLength :: TtlvParser Ttlv
cryptographicLength = tag T.CryptographicLength <+>
                      tint

cryptographicParameters = tag T.CryptographicParameters <+>
                          optional T.BlockCipherMode tenum <+>
                          optional T.PaddingMethod tenum <+>
                          optional T.HashingAlgorithm tenum <+>
                          optional T.KeyRoleType tenum

cryptographicDomainParameters = tag T.CryptographicDomainParameters <+>
                                optional T.Qlength tint <+>
                                optional T.RecommendedCurve tenum

certificateType = tag T.CertificateType <+>
                  tenum

certificateIdentifier = tag      T.CertificateIdentifier <+>
                        apply    T.Issuer tstring <+>
                        optional T.SerialNumber tstring

certificateSubject = tag   T.CertificateSubject <+>
                     apply T.CertificateSubjectDistinguishedName tstring <+>
                     many  T.CertificateSubjectAlternativeName tstring

certificateIssuer = tag   T.CertificateIssuer <+>
                    apply T.CertificateIssuerDistinguishedName tstring <+>
                    many  T.CertificateIssuerAlternativeName tstring

digest = tag T.Digest <+>
         apply T.HashingAlgorithm tenum <+>
         apply T.DigestValue tbytestring

operationPolicyName = tag T.OperationPolicyName <+>
                      tstring

cryptographicUsageMask = tag T.CryptographicUsageMask <+>
                         tint

leaseTime = tag T.LeaseTime <+>
            tinterval

usageLimits = tag T.UsageLimits <+>
              apply T.UsageLimitsTotal tlong <+>
              apply T.UsageLimitsCount tlong <+>
              apply T.UsageLimitsUnit tlong

state = tag T.State <+>
        tenum

initialDate = tag T.InitialDate <+>
              tdatetime

activationDate = tag T.ActivationDate <+>
                 tdatetime

processStartDate = tag T.ProcessStartDate <+>
                   tdatetime

protectStopDate = tag T.ProcessStartDate <+>
                  tdatetime

deactivationDate = tag T.DeactivationDate <+>
                   tdatetime

destroyDate = tag T.DestroyDate <+>
              tdatetime

compromiseOccurrenceDate = tag T.CompromiseOccurrenceDate <+>
                           tdatetime

compromiseDate = tag T.CompromiseDate <+>
                 tdatetime

revocationReason = tag T.RevocationReason <+>
                   apply T.RevocationReasonCode tenum <+>
                   optional T.RevocationMessage tstring

archiveDate = tag T.ArchiveDate <+>
              tdatetime

objectGroup = tag T.ObjectGroup <+>
              tstring

link = tag T.Link <+>
       apply T.LinkType tenum <+>
       apply T.LinkedObjectIdentifier tstring

applicationSpecificInfo = tag T.ApplicationSpecificInformation <+>
                          apply T.ApplicationNamespace tstring <+>
                          apply T.ApplicationData tstring

contactInformation = tag T.ContactInformation <+>
                     tstring

lastChangeDate = tag T.LastChangeDate <+>
                 tdatetime

customAttribute = tag T.CustomAttribute <+>
                  ok -- TODO disallow sub-structures

-- any attribute
any = uniqueIdentifier <|>
      name <|>
      objectType <|>
      cryptographicAlgorithm <|>
      cryptographicLength <|>
      cryptographicParameters <|>
      cryptographicDomainParameters <|>
      certificateType <|>
      certificateIdentifier <|>
      certificateSubject <|>
      certificateIssuer <|>
      digest <|>
      operationPolicyName <|>
      cryptographicUsageMask <|>
      leaseTime <|>
      usageLimits <|>
      state <|>
      initialDate <|>
      activationDate <|>
      processStartDate <|>
      protectStopDate <|>
      deactivationDate <|>
      destroyDate <|>
      compromiseOccurrenceDate <|>
      compromiseDate <|>
      revocationReason <|>
      archiveDate <|>
      objectGroup <|>
      link <|>
      applicationSpecificInfo <|>
      contactInformation <|>
      lastChangeDate <|>
      customAttribute
