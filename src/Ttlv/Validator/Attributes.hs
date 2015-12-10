-- KMIP Attribute
module Ttlv.Validator.Attributes ( uniqueIdentifier
                                 , name
                                 , objectType
                                 , cryptographicAlgorithm
                                 , cryptographicLength
                                 , cryptographicParameters
                                 , cryptographicDomainParameters
                                 , certificateType
                                 , certificateIdentifier
                                 , certificateSubject
                                 , certificateIssuer
                                 , digest
                                 , operationPolicyName
                                 , cryptographicUsageMask
                                 , leaseTime
                                 , usageLimits
                                 , state
                                 , initialDate
                                 , activationDate
                                 , processStartDate
                                 , protectStopDate
                                 , deactivationDate
                                 , destroyDate
                                 , compromiseOccurrenceDate
                                 , compromiseDate
                                 , revocationReason
                                 , archiveDate
                                 , objectGroup
                                 , link
                                 , applicationSpecificInfo
                                 , contactInformation
                                 , lastChangeDate
                                 , customAttribute
                                 , anyAttribute
                                 ) where

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Structures
import           Ttlv.Validator.Types

uniqueIdentifier :: TtlvParser Ttlv
uniqueIdentifier = tag T.UniqueIdentifier <+>
                   tString

name :: TtlvParser Ttlv
name = tag T.Name <+>
       apply T.NameValue tString <+>
       apply T.NameType tEnum

objectType :: TtlvParser Ttlv
objectType = tag T.ObjectType <+>
             tEnum

cryptographicAlgorithm :: TtlvParser Ttlv
cryptographicAlgorithm = tag T.CryptographicAlgorithm <+>
                         tEnum

cryptographicLength :: TtlvParser Ttlv
cryptographicLength = tag T.CryptographicLength <+>
                      tInt

cryptographicParameters = tag T.CryptographicParameters <+>
                          optional T.BlockCipherMode tEnum <+>
                          optional T.PaddingMethod tEnum <+>
                          optional T.HashingAlgorithm tEnum <+>
                          optional T.KeyRoleType tEnum

cryptographicDomainParameters = tag T.CryptographicDomainParameters <+>
                                optional T.Qlength tInt <+>
                                optional T.RecommendedCurve tEnum

certificateType = tag T.CertificateType <+>
                  tEnum

certificateIdentifier = tag      T.CertificateIdentifier <+>
                        apply    T.Issuer tString <+>
                        optional T.SerialNumber tString

certificateSubject = tag   T.CertificateSubject <+>
                     apply T.CertificateSubjectDistinguishedName tString <+>
                     many  T.CertificateSubjectAlternativeName tString

certificateIssuer = tag   T.CertificateIssuer <+>
                    apply T.CertificateIssuerDistinguishedName tString <+>
                    many  T.CertificateIssuerAlternativeName tString

digest = tag T.Digest <+>
         apply T.HashingAlgorithm tEnum <+>
         apply T.DigestValue tByteString

operationPolicyName = tag T.OperationPolicyName <+>
                      tString

cryptographicUsageMask = tag T.CryptographicUsageMask <+>
                         tInt

leaseTime = tag T.LeaseTime <+>
            tInterval

usageLimits = tag T.UsageLimits <+>
              apply T.UsageLimitsTotal tLong <+>
              apply T.UsageLimitsCount tLong <+>
              apply T.UsageLimitsUnit tLong

state = tag T.State <+>
        tEnum

initialDate = tag T.InitialDate <+>
              tDateTime

activationDate = tag T.ActivationDate <+>
                 tDateTime

processStartDate = tag T.ProcessStartDate <+>
                   tDateTime

protectStopDate = tag T.ProcessStartDate <+>
                  tDateTime

deactivationDate = tag T.DeactivationDate <+>
                   tDateTime

destroyDate = tag T.DestroyDate <+>
              tDateTime

compromiseOccurrenceDate = tag T.CompromiseOccurrenceDate <+>
                           tDateTime

compromiseDate = tag T.CompromiseDate <+>
                 tDateTime

revocationReason = tag T.RevocationReason <+>
                   apply T.RevocationReasonCode tEnum <+>
                   optional T.RevocationMessage tString

archiveDate = tag T.ArchiveDate <+>
              tDateTime

objectGroup = tag T.ObjectGroup <+>
              tString

link = tag T.Link <+>
       apply T.LinkType tEnum <+>
       apply T.LinkedObjectIdentifier tString

applicationSpecificInfo = tag T.ApplicationSpecificInformation <+>
                          apply T.ApplicationNamespace tString <+>
                          apply T.ApplicationData tString

contactInformation = tag T.ContactInformation <+>
                     tString

lastChangeDate = tag T.LastChangeDate <+>
                 tDateTime

customAttribute = tag T.CustomAttribute <+>
                  ok -- TODO disallow sub-structures

-- any attribute
anyAttribute = uniqueIdentifier <|>
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
