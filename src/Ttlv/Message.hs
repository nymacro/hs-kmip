module Ttlv.Message where

import qualified Ttlv.Tag as T
import Ttlv.Data
import Ttlv.Structures
import Ttlv.Objects
import Ttlv.Operations

-- Contents
protocolVersion = tag T.ProtocolVersion <+>
                  apply T.ProtocolVersionMajor tint <+>
                  apply T.ProtocolVersionMinor tint

operation = tag T.Operation <+> tenum

maximumResponseSize = tag T.MaximumResponseSize <+> tint

uniqueBatchItemId = tag T.UniqueBatchItemID <+> tbytestring

timeStamp = tag T.TimeStamp <+> tdatetime

authentication = tag T.Authentication <+> credential

asynchronousIndicator = tag T.AsynchronousIndicator <+> tbool

asynchronousCorrelationValue = tag T.AsynchronousCorrelationValue <+> tbytestring

resultStatus = tag T.ResultStatus <+> tenum

resultReason = tag T.ResultReason <+> tenum

resultMessage = tag T.ResultMessage <+> tstring

batchOrderOption = tag T.BatchOrderOption <+> tbool

batchErrorContinuationOption = tag T.BatchErrorContinuationOption <+> tenum

batchCount = tag T.BatchCount <+> tint

batchItem = tag T.BatchItem <+> tstruct

messageExtension = tag T.MessageExtension <+>
                   apply T.VendorIdentification tstring <+>
                   apply T.CriticalityIndicator tbool <+>
                   apply T.VendorExtension tstruct

-- Format
requestMessage = tag T.RequestMessage <+> tstruct <+>
                 apply T.RequestHeader requestHeader <+>
                 many1 T.BatchItem requestBatchItem

responseMessage = tag T.ResponseMessage <+> tstruct <+>
                  apply T.ResponseHeader responseHeader <+>
                  many1 T.BatchItem responseBatchItem

requestHeader = tag T.RequestHeader <+>
                apply T.ProtocolVersion protocolVersion <+>
                optional T.MaximumResponseSize  maximumResponseSize <+>
                optional T.AsynchronousIndicator asynchronousIndicator <+>
                optional T.Authentication authentication <+>
                optional T.BatchErrorContinuationOption batchErrorContinuationOption <+>
                optional T.BatchOrderOption batchOrderOption <+>
                optional T.TimeStamp timeStamp <+>
                apply T.BatchCount batchCount

requestBatchItem = tag T.BatchItem <+> tstruct <+>
                   apply T.Operation operation <+>
                   optional T.UniqueBatchItemID uniqueBatchItemId <+>
                   apply T.RequestPayload requestOperation -- FIXME need to run specific
--                   applyIf request T.RequestPayload
--  where op = runTtlvParser $ tag T.Opperation x
--        request = requestOperationFor op

responseHeader = tag T.ResponseHeader <+>
                 apply T.ProtocolVersion protocolVersion <+>
                 apply T.TimeStamp timeStamp <+>
                 apply T.BatchCount batchCount

responseBatchItem = tag T.BatchItem <+> tstruct <+>
                    apply T.Operation operation <+>
                    optional T.UniqueBatchItemID uniqueBatchItemId <+>
                    apply T.ResultStatus resultStatus <+>
                    optional T.ResultReason resultReason <+>
                    optional T.ResultMessage resultMessage <+>
                    optional T.AsynchronousCorrelationValue asynchronousCorrelationValue <+>
                    optional T.ResponsePayload responseOperation <+>
                    optional T.MessageExtension messageExtension
