{-# LANGUAGE OverloadedStrings #-}
module Ttlv ( encodeTtlv
            , decodeTtlv
            , Ttlv
            , TtlvData
            , TtlvTag ) where

import Control.Applicative
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
-- import Data.Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import System.Locale
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTime)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Base16 as B16
import Numeric (showHex)
import qualified Data.ByteString.Internal as BS (w2c, c2w)
import Test.Hspec
import Data.Maybe

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

data TtlvVersion = TtlvVersion Int Int deriving (Show, Eq)
-- ttlvTagVersion :: TtlvTag -> TtlvVersion

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv TtlvTag TtlvData
            deriving (Show, Eq)

instance Binary Ttlv where
  get = parseTtlv
  put = unparseTtlv

-- | Tag data
data TtlvData = TtlvStructure [Ttlv]
              | TtlvInt Int
              | TtlvLongInt Integer
              | TtlvBigInt Integer
              | TtlvEnum Int
              | TtlvBool Bool
              | TtlvString String
              | TtlvByteString L.ByteString
              | TtlvDateTime UTCTime
              | TtlvInterval Int
              deriving (Show, Eq)

parseTtlvStructure' :: Get [Ttlv]
parseTtlvStructure' = do
  ttlv <- parseTtlv
  rema <- remaining
  if rema == 0
  then return [ttlv]
  else do
    rest <- parseTtlvStructure'
    return $ ttlv : rest

parseTtlvStructure :: Get TtlvData
parseTtlvStructure = do
  ttlvs <- parseTtlvStructure'
  return $ TtlvStructure ttlvs

parseTtlvInt :: Get TtlvData
parseTtlvInt = do
  val <- getWord32be
  return $ TtlvInt $ fromIntegral val

parseTtlvLongInt :: Get TtlvData
parseTtlvLongInt = do
  val <- getWord64be
  return $ TtlvLongInt $ fromIntegral val

-- FIXME this isn't implemented
parseTtlvBigInt :: Get TtlvData
parseTtlvBigInt = do
  val <- getRemainingLazyByteString
--  return $ TtlvBigInt $ fromIntegral val
  return $ TtlvBigInt 0

parseTtlvEnum :: Get TtlvData
parseTtlvEnum = do
  val <- getWord32be
  return $ TtlvEnum $ fromIntegral val

parseTtlvBool :: Get TtlvData
parseTtlvBool = do
  val <- getWord64be
  return $ TtlvBool (val == 1)

-- FIXME this isn't implemented
parseTtlvString :: Get TtlvData
parseTtlvString = do
  val <- getRemainingLazyByteString
  return $ TtlvString $ map BS.w2c $ L.unpack val

parseTtlvByteString :: Get TtlvData
parseTtlvByteString = do
  val <- getRemainingLazyByteString
  return $ TtlvByteString val

parseTtlvDateTime :: Get TtlvData
parseTtlvDateTime = do
  val <- getWord64be
  return $ TtlvDateTime $ posixSecondsToUTCTime $ fromIntegral val

parseTtlvInterval :: Get TtlvData
parseTtlvInterval = do
  val <- getWord32be
  return $ TtlvInterval $ fromIntegral val

decodeTtlvTag :: L.ByteString -> Int
decodeTtlvTag x = fromIntegral (decode (0 `L.cons'` x) :: Word32)

encodeTtlvTag :: Int -> L.ByteString
encodeTtlvTag x = snd $ fromJust $ L.uncons $ encode (fromIntegral x :: Word32)

prettyPrint :: L.ByteString -> String
prettyPrint = concat . map (flip showHex "-") . L.unpack

parseTtlv :: Get Ttlv
parseTtlv = do
  tag <- getLazyByteString 3
  typ <- getWord8
  len <- getWord32be
  val <- getLazyByteString $ fromIntegral len
  when (fromIntegral typ `elem` [2, 5, 10]) $ skip 4 -- skip padding
  return $ Ttlv (toTtlvTag $ decodeTtlvTag tag)
             (case fromIntegral typ of
                1  -> runGet parseTtlvStructure val
                2  -> runGet parseTtlvInt val
                3  -> runGet parseTtlvLongInt val
                4  -> runGet parseTtlvBigInt val
                5  -> runGet parseTtlvEnum val
                6  -> runGet parseTtlvBool val
                7  -> runGet parseTtlvString val
                8  -> runGet parseTtlvByteString val
                9  -> runGet parseTtlvDateTime val
                10 -> runGet parseTtlvInterval val
                otherwise -> error "unknown type")

-- | Retrieve the corresponding ID for TtlvData
ttlvDataType :: TtlvData -> Int
ttlvDataType (TtlvStructure _) = 1
ttlvDataType (TtlvInt _) = 2
ttlvDataType (TtlvLongInt _) = 3
ttlvDataType (TtlvBigInt _) = 4
ttlvDataType (TtlvEnum _) = 5
ttlvDataType (TtlvBool _) = 6
ttlvDataType (TtlvString _) = 7
ttlvDataType (TtlvByteString _) = 8
ttlvDataType (TtlvDateTime _) = 9
ttlvDataType (TtlvInterval _) = 10

ttlvDataLength :: TtlvData -> Int
ttlvDataLength (TtlvStructure s) = foldr1 (+) $ map ttlvLength s
ttlvDataLength (TtlvInt _) = 8 -- w/padding
ttlvDataLength (TtlvLongInt _) = 8
ttlvDataLength (TtlvBigInt _) = undefined
ttlvDataLength (TtlvEnum _) = 8 -- w/padding
ttlvDataLength (TtlvBool _) = 8
ttlvDataLength (TtlvString x) = length x
ttlvDataLength (TtlvByteString x) = fromIntegral $ L.length x
ttlvDataLength (TtlvDateTime _) = 8
ttlvDataLength (TtlvInterval _) = 8 -- w/padding

ttlvLength :: Ttlv -> Int
ttlvLength (Ttlv t d) = 3 + 1 + 4 + (ttlvDataLength d)

unparseTtlvData :: TtlvData -> Put
unparseTtlvData (TtlvStructure x) = mapM_ unparseTtlv x
unparseTtlvData (TtlvInt x) = do
  putWord32be $ fromIntegral x
  putWord32be 0
unparseTtlvData (TtlvLongInt x) = putWord64be $ fromIntegral x
-- unparseTtlvdata (TtlvBigInt x) = fail "Undefined encode for Big Integer"
unparseTtlvData (TtlvEnum x) = do
  putWord32be $ fromIntegral x
  putWord32be 0
unparseTtlvData (TtlvBool x) = if x
                               then putWord64be 1
                               else putWord64be 0
unparseTtlvData (TtlvString x) = putLazyByteString $ L.pack $ map BS.c2w x 
unparseTtlvData (TtlvByteString x) = putLazyByteString x
unparseTtlvData (TtlvDateTime x) = putWord64be $ fromIntegral $ round $ utcTimeToPOSIXSeconds x
unparseTtlvData (TtlvInterval x) = do
  putWord32be $ fromIntegral x
  putWord32be 0

unparseTtlv :: Ttlv -> Put
unparseTtlv (Ttlv t d) = do
  putLazyByteString $ encodeTtlvTag $ fromTtlvTag t
  putWord8 $ fromIntegral $ ttlvDataType d
  -- this is terrible. Find a better way to do this
  let length = ttlvDataLength d
      real_length = if ttlvDataType d `elem` [2, 5, 10]
                    then length - 4
                    else length
  putWord32be $ fromIntegral $ real_length
  unparseTtlvData d

fromHex x = L.fromChunks [fst $ B16.decode x]

exampleTtlvs = [ "42002002000000040000000800000000" -- Integer
               , "420020030000000801B69B4BA5749200" -- Long Integer
               , "42002004000000100000000003FD35EB6BC2DF4618080000" --BigInt
               , "4200200500000004000000FF00000000" -- Enum
               , "42002006000000080000000000000001" -- Bool
               , "420020070000000B48656C6C6F20576F726C640000000000" -- String
               , "42002008000000030102030000000000" -- Byte String
               , "42002009000000080000000047DA67F8" -- Date Time
               , "4200200A00000004000D2F0000000000" -- Interval
               , "42002001000000204200040500000004000000FE000000004200050200000004000000FF00000000" -- Structure
               ]
testTtlvs = map fromHex exampleTtlvs

-- | Decode a Lazy ByteString into the corresponding Ttlv type
decodeTtlv :: L.ByteString -> Ttlv
decodeTtlv = runGet parseTtlv


encodeTtlv :: Ttlv -> L.ByteString
encodeTtlv x = runPut $ unparseTtlv x
  
test :: IO ()
test = do
  hspec $ do
    describe "Ttlv" $ do
      describe "Examples" $ do
        describe "Encode" $ do
          it "should encode/decode Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvInt 8))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Long Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvLongInt 123456789000000000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Big Integer" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvBigInt 1234567890000000000000000000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Enumeration" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvEnum 255))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Boolean" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvBool True))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode String" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode ByteString" $ do
            pending
          it "should encode/decode DateTime" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Interval" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvInterval 864000))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
          it "should encode/decode Structure" $ do
            let t = (Ttlv TtlvCompromiseDate (TtlvStructure [Ttlv TtlvApplicationSpecificInformation (TtlvEnum 254),Ttlv TtlvArchiveDate (TtlvInt 255)]))
            (decodeTtlv $ encodeTtlv t) `shouldBe` t
            
        describe "Decode" $ do
          it "should decode Integer" $ do
            decodeTtlv (testTtlvs !! 0) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvInt 8))
          it "should decode Long Integer" $ do
            decodeTtlv (testTtlvs !! 1) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvLongInt 123456789000000000))
          it "should decode Big Integer" $ do
            decodeTtlv (testTtlvs !! 2) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvBigInt 1234567890000000000000000000))
          it "should decode Enumeration" $ do
            decodeTtlv (testTtlvs !! 3) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvEnum 255))
          it "should decode Boolean" $ do
            decodeTtlv (testTtlvs !! 4) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvBool True))
          it "should decode String" $ do
            decodeTtlv (testTtlvs !! 5) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvString "Hello World"))
          it "should decode ByteString" $ do
            pending
            -- decodeTtlv (testTtlvs !! 6) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvByteString undefined))
          it "should decode DateTime" $ do
            decodeTtlv (testTtlvs !! 7) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvDateTime (fromJust $ parseTime defaultTimeLocale "%F %T %Z" "2008-03-14 11:56:40 UTC")))
          it "should decode Interval" $ do
            decodeTtlv (testTtlvs !! 8) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvInterval 864000))
          it "should decode Structure" $ do
            decodeTtlv (testTtlvs !! 9) `shouldBe` (Ttlv TtlvCompromiseDate (TtlvStructure [Ttlv TtlvApplicationSpecificInformation (TtlvEnum 254),Ttlv TtlvArchiveDate (TtlvInt 255)]))

main :: IO ()
main = do
  mapM_ (putStrLn . show . decodeTtlv) testTtlvs
