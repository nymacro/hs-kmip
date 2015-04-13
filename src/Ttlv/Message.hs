module Ttlv.Message where

import qualified Ttlv.Tag as T
import Ttlv.Data
import Ttlv.Structures
import Ttlv.Objects
import Ttlv.Operations

-- Contents
protocolVersion = do
  tag T.ProtocolVersion 
  apply T.ProtocolVersionMajor tint 
  apply T.ProtocolVersionMinor tint

operation = do
  tag T.Operation
  tenum

maximumResponseSize = do
  tag T.MaximumResponseSize
  tint

uniqueBatchItemId = do
  tag T.UniqueBatchItemID
  tbytestring

timeStamp = do
  tag T.TimeStamp
  tdatetime

authentication = do
  tag T.Authentication <+> credential

asynchronousIndicator = do
  tag T.AsynchronousIndicator <+> tbool

asynchronousCorrelationValue = do
  tag T.AsynchronousCorrelationValue <+> tbytestring

resultStatus = do
  tag T.ResultStatus <+> tenum

resultReason = do
  tag T.ResultReason <+> tenum

resultMessage = do
  tag T.ResultMessage <+> tstring

batchOrderOption = do
  tag T.BatchOrderOption <+> tbool

batchErrorContinuationOption = do
  tag T.BatchErrorContinuationOption <+> tenum

batchCount = do
  tag T.BatchCount <+> tint

batchItem = do
  tag T.BatchItem <+> tstruct

messageExtension = do
  tag T.MessageExtension 
  apply T.VendorIdentification tstring 
  apply T.CriticalityIndicator tbool 
  apply T.VendorExtension tstruct

-- Format
requestMessage = do
  tag T.RequestMessage
  tstruct 
  apply T.RequestHeader requestHeader 
  many1 T.BatchItem requestBatchItem

responseMessage = do
  tag T.ResponseMessage
  tstruct 
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
  tstruct
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
  tstruct
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
    _ -> nok $ "unexpected operation type (got " ++ show op ++ ")"
  optional T.MessageExtension messageExtension
