module Ttlv.Tag where

data TtlvVersion = TtlvVersion Int Int deriving (Show, Eq)
-- ttlvTagVersion :: TtlvTag -> TtlvVersion

-- | Tag identifier
data TtlvTag = TtlvActivationDate
             | TtlvApplicationData
             | TtlvApplicationNamespace
             | TtlvApplicationSpecificInformation
             | TtlvArchiveDate
             | TtlvAsynchronousCorrelationValue
             | TtlvAsynchronousIndicator
             | TtlvAttribute 
             | TtlvAttributeIndex 
             | TtlvAttributeName 
             | TtlvAttributeValue 
             | TtlvAuthentication 
             | TtlvBatchCount 
             | TtlvBatchErrorContinuationOption 
             | TtlvBatchItem 
             | TtlvBatchOrderOption 
             | TtlvBlockCipherMode 
             | TtlvCancellationResult 
             | TtlvCertificate 
             | TtlvCertificateIdentifier 
             | TtlvCertificateIssuer 
             | TtlvCertificateIssuerAlternativeName 
             | TtlvCertificateIssuerDistinguishedName 
             | TtlvCertificateRequest 
             | TtlvCertificateRequestType 
             | TtlvCertificateSubject 
             | TtlvCertificateSubjectAlternativeName 
             | TtlvCertificateSubjectDistinguishedName 
             | TtlvCertificateType 
             | TtlvCertificateValue 
             | TtlvCommonTemplateAttribute 
             | TtlvCompromiseDate 
             | TtlvCompromiseOccurrenceDate 
             | TtlvContactInformation 
             | TtlvCredential 
             | TtlvCredentialType 
             | TtlvCredentialValue 
             | TtlvCriticalityIndicator 
             | TtlvCRTCoefficient 
             | TtlvCryptographicAlgorithm 
             | TtlvCryptographicDomainParameters 
             | TtlvCryptographicLength 
             | TtlvCryptographicParameters 
             | TtlvCryptographicUsageMask 
             | TtlvCustomAttribute 
             | TtlvD 
             | TtlvDeactivationDate 
             | TtlvDerivationData 
             | TtlvDerivationMethod 
             | TtlvDerivationParameters 
             | TtlvDestroyDate 
             | TtlvDigest 
             | TtlvDigestValue 
             | TtlvEncryptionKeyInformation 
             | TtlvG 
             | TtlvHashingAlgorithm 
             | TtlvInitialDate 
             | TtlvInitializationVector 
             | TtlvIssuer 
             | TtlvIterationCount 
             | TtlvIVCounterNonce 
             | TtlvJ 
             | TtlvKey 
             | TtlvKeyBlock 
             | TtlvKeyCompressionType 
             | TtlvKeyFormatType 
             | TtlvKeyMaterial 
             | TtlvKeyPartIdentifier 
             | TtlvKeyValue 
             | TtlvKeyWrappingData 
             | TtlvKeyWrappingSpecification 
             | TtlvLastChangeDate 
             | TtlvLeaseTime 
             | TtlvLink 
             | TtlvLinkType 
             | TtlvLinkedObjectIdentifier 
             | TtlvMACSignature 
             | TtlvMACSignatureKeyInformation 
             | TtlvMaximumItems 
             | TtlvMaximumResponseSize 
             | TtlvMessageExtension 
             | TtlvModulus 
             | TtlvName 
             | TtlvNameType 
             | TtlvNameValue 
             | TtlvObjectGroup 
             | TtlvObjectType 
             | TtlvOffset 
             | TtlvOpaqueDataType 
             | TtlvOpaqueDataValue 
             | TtlvOpaqueObject 
             | TtlvOperation 
             | TtlvOperationPolicyName 
             | TtlvP 
             | TtlvPaddingMethod 
             | TtlvPrimeExponentP 
             | TtlvPrimeExponentQ 
             | TtlvPrimeFieldSize 
             | TtlvPrivateExponent 
             | TtlvPrivateKey 
             | TtlvPrivateKeyTemplateAttribute 
             | TtlvPrivateKeyUniqueIdentifier 
             | TtlvProcessStartDate
             | TtlvProtectStopDate
             | TtlvProtocolVersion
             | TtlvProtocolVersionMajor
             | TtlvProtocolVersionMinor
             | TtlvPublicExponent
             | TtlvPublicKey
             | TtlvPublicKeyTemplateAttribute
             | TtlvPublicKeyUniqueIdentifier
             | TtlvPutFunction
             | TtlvQ
             | TtlvQString
             | TtlvQlength
             | TtlvQueryFunction
             | TtlvRecommendedCurve
             | TtlvReplacedUniqueIdentifier
             | TtlvRequestHeader
             | TtlvRequestMessage
             | TtlvRequestPayload
             | TtlvResponseHeader
             | TtlvResponseMessage
             | TtlvResponsePayload
             | TtlvResultMessage
             | TtlvResultReason
             | TtlvResultStatus
             | TtlvRevocationMessage
             | TtlvRevocationReason
             | TtlvRevocationReasonCode
             | TtlvKeyRoleType
             | TtlvSalt
             | TtlvSecretData
             | TtlvSecretDataType
             | TtlvSerialNumber
             | TtlvServerInformation
             | TtlvSplitKey
             | TtlvSplitKeyMethod
             | TtlvSplitKeyParts
             | TtlvSplitKeyThreshold
             | TtlvState
             | TtlvStorageStatusMask
             | TtlvSymmetricKey
             | TtlvTemplate
             | TtlvTemplateAttribute
             | TtlvTimeStamp
             | TtlvUniqueBatchItemID
             | TtlvUniqueIdentifier
             | TtlvUsageLimits
             | TtlvUsageLimitsCount
             | TtlvUsageLimitsTotal
             | TtlvUsageLimitsUnit
             | TtlvUsername
             | TtlvValidityDate
             | TtlvValidityIndicator
             | TtlvVendorExtension
             | TtlvVendorIdentification
             | TtlvWrappingMethod
             | TtlvX
             | TtlvY
             | TtlvPassword
             -- KMIP 1.1 (from 4200A2 to 4200B7)
             | TtlvDeviceIdentifier
             | TtlvEncodingOption
             | TtlvExtensionInformation 
             | TtlvExtensionName 
             | TtlvExtensionTag 
             | TtlvExtensionType 
             | TtlvFresh 
             | TtlvMachineIdentifier 
             | TtlvMediaIdentifier 
             | TtlvNetworkIdentifier 
             | TtlvObjectGroupMember 
             | TtlvCertificateLength 
             | TtlvDigitalSignatureAlgorithm 
             | TtlvCertificateSerialNumber 
             | TtlvDeviceSerialNumber 
             | TtlvIssuerAlternativeName 
             | TtlvIssuerDistinguishedName 
             | TtlvSubjectAlternativeName 
             | TtlvSubjectDistinguishedName 
             | TtlvX509CertificateIdentifier 
             | TtlvX509CertificateIssuer 
             | TtlvX509CertificateSubject 
             -- KMIP 1.2 (from 4200B8 to 4200D3)
             | TtlvKeyValueLocation
             | TtlvKeyValueLocationValue
             | TtlvKeyValueLocationType
             | TtlvKeyValuePresent
             | TtlvOriginalCreationDate
             | TtlvPGPKey
             | TtlvPGPKeyVersion
             | TtlvAlternativeName
             | TtlvAlternativeNameValue
             | TtlvAlternativeNameType
             | TtlvData
             | TtlvSignatureData
             | TtlvDataLength
             | TtlvRandomIV
             | TtlvMACData
             | TtlvAttestationType
             | TtlvNonce
             | TtlvNonceID
             | TtlvNonceValue
             | TtlvAttestationMeasurement
             | TtlvAttestationAssertion
             | TtlvIVLength
             | TtlvTagLength
             | TtlvFixedFieldLength
             | TtlvCounterLength
             | TtlvInitialCounterValue
             | TtlvInvocationFieldLength
             | TtlvAttestationCapableIndicator
             deriving (Show, Eq, Enum)

fromTtlvTag :: TtlvTag -> Int
fromTtlvTag x = 0x420001 + fromEnum x

toTtlvTag :: Int -> TtlvTag
toTtlvTag x = toEnum $ x - 0x420001

-- helper function
ttlvTagVersion' :: Int -> TtlvVersion
ttlvTagVersion' x | x >= 0x420001 && x <= 0x4200A1 = TtlvVersion 1 0
                  | x >= 0x420001 && x <= 0x4200B7 = TtlvVersion 1 1
                  | x >= 0x420001 && x <= 0x4200D3 = TtlvVersion 1 2
                  | otherwise    = undefined

ttlvTagVersion :: TtlvTag -> TtlvVersion
ttlvTagVersion = ttlvTagVersion' . fromTtlvTag

