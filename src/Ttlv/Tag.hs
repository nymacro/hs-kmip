{-# LANGUAGE DeriveFunctor #-}
module Ttlv.Tag ( TtlvVersion
                , TtlvTag(..)
                , Tag
                , Tag'(..)
                , fromTtlvTag
                , toTtlvTag
                , toTag
                , fromTag ) where

data TtlvVersion = TtlvVersion Int Int deriving (Show, Eq)

-- | Tag identifier
data TtlvTag = ActivationDate
             | ApplicationData
             | ApplicationNamespace
             | ApplicationSpecificInformation
             | ArchiveDate
             | AsynchronousCorrelationValue
             | AsynchronousIndicator
             | Attribute
             | AttributeIndex
             | AttributeName
             | AttributeValue
             | Authentication
             | BatchCount
             | BatchErrorContinuationOption
             | BatchItem
             | BatchOrderOption
             | BlockCipherMode
             | CancellationResult
             | Certificate
             | CertificateIdentifier
             | CertificateIssuer
             | CertificateIssuerAlternativeName
             | CertificateIssuerDistinguishedName
             | CertificateRequest
             | CertificateRequestType
             | CertificateSubject
             | CertificateSubjectAlternativeName
             | CertificateSubjectDistinguishedName
             | CertificateType
             | CertificateValue
             | CommonTemplateAttribute
             | CompromiseDate
             | CompromiseOccurrenceDate
             | ContactInformation
             | Credential
             | CredentialType
             | CredentialValue
             | CriticalityIndicator
             | CRTCoefficient
             | CryptographicAlgorithm
             | CryptographicDomainParameters
             | CryptographicLength
             | CryptographicParameters
             | CryptographicUsageMask
             | CustomAttribute
             | D
             | DeactivationDate
             | DerivationData
             | DerivationMethod
             | DerivationParameters
             | DestroyDate
             | Digest
             | DigestValue
             | EncryptionKeyInformation
             | G
             | HashingAlgorithm
             | InitialDate
             | InitializationVector
             | Issuer
             | IterationCount
             | IVCounterNonce
             | J
             | Key
             | KeyBlock
             | KeyCompressionType
             | KeyFormatType
             | KeyMaterial
             | KeyPartIdentifier
             | KeyValue
             | KeyWrappingData
             | KeyWrappingSpecification
             | LastChangeDate
             | LeaseTime
             | Link
             | LinkType
             | LinkedObjectIdentifier
             | MACSignature
             | MACSignatureKeyInformation
             | MaximumItems
             | MaximumResponseSize
             | MessageExtension
             | Modulus
             | Name
             | NameType
             | NameValue
             | ObjectGroup
             | ObjectType
             | Offset
             | OpaqueDataType
             | OpaqueDataValue
             | OpaqueObject
             | Operation
             | OperationPolicyName
             | P
             | PaddingMethod
             | PrimeExponentP
             | PrimeExponentQ
             | PrimeFieldSize
             | PrivateExponent
             | PrivateKey
             | PrivateKeyTemplateAttribute
             | PrivateKeyUniqueIdentifier
             | ProcessStartDate
             | ProtectStopDate
             | ProtocolVersion
             | ProtocolVersionMajor
             | ProtocolVersionMinor
             | PublicExponent
             | PublicKey
             | PublicKeyTemplateAttribute
             | PublicKeyUniqueIdentifier
             | PutFunction
             | Q
             | QString
             | Qlength
             | QueryFunction
             | RecommendedCurve
             | ReplacedUniqueIdentifier
             | RequestHeader
             | RequestMessage
             | RequestPayload
             | ResponseHeader
             | ResponseMessage
             | ResponsePayload
             | ResultMessage
             | ResultReason
             | ResultStatus
             | RevocationMessage
             | RevocationReason
             | RevocationReasonCode
             | KeyRoleType
             | Salt
             | SecretData
             | SecretDataType
             | SerialNumber
             | ServerInformation
             | SplitKey
             | SplitKeyMethod
             | SplitKeyParts
             | SplitKeyThreshold
             | State
             | StorageStatusMask
             | SymmetricKey
             | Template
             | TemplateAttribute
             | TimeStamp
             | UniqueBatchItemID
             | UniqueIdentifier
             | UsageLimits
             | UsageLimitsCount
             | UsageLimitsTotal
             | UsageLimitsUnit
             | Username
             | ValidityDate
             | ValidityIndicator
             | VendorExtension
             | VendorIdentification
             | WrappingMethod
             | X
             | Y
             | Password
             -- KMIP 1.1 (from 4200A2 to 4200B7)
             | DeviceIdentifier
             | EncodingOption
             | ExtensionInformation
             | ExtensionName
             | ExtensionTag
             | ExtensionType
             | Fresh
             | MachineIdentifier
             | MediaIdentifier
             | NetworkIdentifier
             | ObjectGroupMember
             | CertificateLength
             | DigitalSignatureAlgorithm
             | CertificateSerialNumber
             | DeviceSerialNumber
             | IssuerAlternativeName
             | IssuerDistinguishedName
             | SubjectAlternativeName
             | SubjectDistinguishedName
             | X509CertificateIdentifier
             | X509CertificateIssuer
             | X509CertificateSubject
             -- KMIP 1.2 (from 4200B8 to 4200D3)
             | KeyValueLocation
             | KeyValueLocationValue
             | KeyValueLocationType
             | KeyValuePresent
             | OriginalCreationDate
             | PGPKey
             | PGPKeyVersion
             | AlternativeName
             | AlternativeNameValue
             | AlternativeNameType
             | Data
             | SignatureData
             | DataLength
             | RandomIV
             | MACData
             | AttestationType
             | Nonce
             | NonceID
             | NonceValue
             | AttestationMeasurement
             | AttestationAssertion
             | IVLength
             | TagLength
             | FixedFieldLength
             | CounterLength
             | InitialCounterValue
             | InvocationFieldLength
             | AttestationCapableIndicator
             deriving (Show, Eq, Enum, Bounded)

data Tag' a = Tag TtlvTag
            | Extension a
            | Unknown a
            deriving (Show, Eq, Functor)
type Tag = Tag' Int

-- data Tag = Tag TtlvTag
--          | Extension Int
--          | Unknown Int
--          deriving (Show, Eq)

fromTtlvTag :: TtlvTag -> Int
fromTtlvTag x = 0x420001 + fromEnum x

toTtlvTag :: Int -> TtlvTag
toTtlvTag x = toEnum $ x - 0x420001

toTag :: Int -> Tag
toTag x | x >= 0x540000 && x <= 0x54FFFF = Extension x
        | x >= 0x420001 && x <= 0x4200D3 = Tag $ toTtlvTag x
        | otherwise = Unknown x

fromTag :: Tag -> Int
fromTag (Tag x)       = fromTtlvTag x
fromTag (Unknown x)   = x
fromTag (Extension x) = x

-- helper function
ttlvTagVersion' :: Int -> Maybe TtlvVersion
ttlvTagVersion' x | x >= 0x420001 && x <= 0x4200A1 = Just $ TtlvVersion 1 0
                  | x >= 0x420001 && x <= 0x4200B7 = Just $ TtlvVersion 1 1
                  | x >= 0x420001 && x <= 0x4200D3 = Just $ TtlvVersion 1 2
                  | otherwise    = Nothing

ttlvTagVersion :: TtlvTag -> Maybe TtlvVersion
ttlvTagVersion = ttlvTagVersion' . fromTtlvTag

