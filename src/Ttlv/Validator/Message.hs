module Ttlv.Validator.Message where

import           Ttlv.Data
import qualified Ttlv.Tag                  as T
import           Ttlv.Lens
import           Ttlv.Validator.Objects
import           Ttlv.Validator.Operations
import           Ttlv.Validator.Structures
import           Ttlv.Validator.Types

import           Control.Monad             (when)

-- Contents
protocolVersion = do
  tag T.ProtocolVersion
  apply T.ProtocolVersionMajor tInt
  apply T.ProtocolVersionMinor tInt

  major <- get T.ProtocolVersionMajor
  minor <- get T.ProtocolVersionMinor
  with _TtlvInt major $ nokIf (> 1) "protocol major version invalid"
  with _TtlvInt minor $ nokIf (> 0) "protocol minor version invalid"

operation = do
  tag T.Operation
  tEnum

maximumResponseSize = do
  tag T.MaximumResponseSize
  tInt

uniqueBatchItemId = do
  tag T.UniqueBatchItemID
  tByteString

timeStamp = do
  tag T.TimeStamp
  tDateTime

authentication = do
  tag T.Authentication <+> credential

asynchronousIndicator = do
  tag T.AsynchronousIndicator <+> tBool

asynchronousCorrelationValue = do
  tag T.AsynchronousCorrelationValue <+> tByteString

resultStatus = do
  tag T.ResultStatus <+> tEnum

resultReason = do
  tag T.ResultReason <+> tEnum

resultMessage = do
  tag T.ResultMessage <+> tString

batchOrderOption = do
  tag T.BatchOrderOption <+> tBool

batchErrorContinuationOption = do
  tag T.BatchErrorContinuationOption <+> tEnum

batchCount = do
  tag T.BatchCount <+> tInt

batchItem = do
  tag T.BatchItem <+> tStruct

messageExtension = do
  tag T.MessageExtension
  apply T.VendorIdentification tString
  apply T.CriticalityIndicator tBool
  apply T.VendorExtension tStruct

-- Format
requestMessage = do
  tag T.RequestMessage
  tStruct
  apply T.RequestHeader requestHeader
  many1 T.BatchItem requestBatchItem

responseMessage = do
  tag T.ResponseMessage
  tStruct
  apply T.ResponseHeader responseHeader
  many1 T.BatchItem responseBatchItem

requestHeader = do
  tag T.RequestHeader
  apply T.ProtocolVersion protocolVersion
  optional T.MaximumResponseSize  maximumResponseSize
  optional T.AsynchronousIndicator asynchronousIndicator
  optional T.Authentication authentication
  optional T.BatchErrorContinuationOption batchErrorContinuationOption
  optional T.BatchOrderOption batchOrderOption
  optional T.TimeStamp timeStamp
  apply T.BatchCount batchCount

requestBatchItem = do
  tag T.BatchItem
  tStruct
  -- apply T.Operation operation
  op <- get T.Operation
  optional T.UniqueBatchItemID uniqueBatchItemId
  case op of
    TtlvEnum op' -> case requestOperationFor op' of
      Just x  -> apply T.RequestPayload x
      Nothing -> nok $ "unhandled operation type (" ++ show op ++ ")"
    _ -> nok $ "unexpected operation type (got " ++ show op ++ ")"

responseHeader = do
  tag T.ResponseHeader
  apply T.ProtocolVersion protocolVersion
  apply T.TimeStamp timeStamp
  apply T.BatchCount batchCount

responseBatchItem = do
  tag T.BatchItem
  tStruct
  -- apply T.Operation operation
  op <- get T.Operation
  optional T.UniqueBatchItemID uniqueBatchItemId
  apply T.ResultStatus resultStatus
  optional T.ResultReason resultReason
  optional T.ResultMessage resultMessage
  optional T.AsynchronousCorrelationValue asynchronousCorrelationValue
  -- TODO don't always have payload optional for everything
  case op of
    TtlvEnum op' -> case responseOperationFor op' of
      Just x  -> optional T.ResponsePayload x
      Nothing -> nok $ "unhandled operation type (" ++ show op ++ ")"
    otherwise -> nok $ "unexpected operation type (got " ++ show op ++ ")"
  optional T.MessageExtension messageExtension
