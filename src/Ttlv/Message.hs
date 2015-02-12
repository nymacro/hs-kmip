module Ttlv.Message where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Structures
import Ttlv.Objects

-- Contents
protocolVersion = tag TtlvProtocolVersion <+>
                  apply TtlvProtocolVersionMajor tint <+>
                  apply TtlvProtocolVersionMinor tint

operation = tag TtlvOperation <+>
            tenum

maximumResponseSize = tag TtlvMaximumResponseSize <+>
                      tint

uniqueBatchItemId = tag TtlvUniqueBatchItemID <+>
                    tint

timeStamp = tag TtlvTimeStamp <+>
            tdatetime

authentication = tag TtlvAuthentication <+>
                 credential


asynchronousIndicator = tag TtlvAsynchronousIndicator <+>
                        tbool

asynchronousCorrelationValue = tag TtlvAsynchronousCorrelationValue <+>
                               tbytestring

resultStatus = tag TtlvResultStatus <+>
               tenum

resultReason = tag TtlvResultReason <+>
               tenum

resultMessage = tag TtlvResultMessage <+>
                tstring

batchOrderOption = tag TtlvBatchOrderOption <+>
                   tbool


batchErrorContinuationOption = tag TtlvBatchErrorContinuationOption <+>
                               tenum

batchCount = tag TtlvBatchCount <+>
             tint

batchItem = tag TtlvBatchItem <+>
            tstruct

messageExtension = tag TtlvMessageExtension <+>
                   apply TtlvVendorIdentification tstring <+>
                   apply TtlvCriticalityIndicator tbool <+>
                   apply TtlvVendorExtension tstruct

-- Format
requestMessage = tag TtlvRequestMessage <+> tstruct <+>
                 apply TtlvRequestHeader requestHeader <+>
                 many1 TtlvBatchItem batchItem

responseMessage = tag TtlvResponseMessage <+> tstruct <+>
                  apply TtlvResponseHeader responseHeader <+>
                  many1 TtlvBatchItem batchItem

requestHeader = tag TtlvRequestHeader <+>
                apply TtlvProtocolVersion protocolVersion <+>
                optional TtlvMaximumResponseSize  maximumResponseSize <+>
                optional TtlvAsynchronousIndicator asynchronousIndicator <+>
                optional TtlvAuthentication authentication <+>
                optional TtlvBatchErrorContinuationOption batchErrorContinuationOption <+>
                optional TtlvBatchOrderOption batchOrderOption <+>
                optional TtlvTimeStamp timeStamp <+>
                apply TtlvBatchCount batchCount

requestBatchItem = tag TtlvBatchItem <+>
                   apply TtlvOperation operation <+>
                   optional TtlvUniqueBatchItemID uniqueBatchItemId <+>
                   apply TtlvRequestPayload tstruct

responseHeader = tag TtlvResponseHeader <+>
                 apply TtlvProtocolVersion protocolVersion <+>
                 apply TtlvTimeStamp timeStamp <+>
                 apply TtlvBatchCount batchCount

responseBatchItem = tag TtlvBatchItem <+>
                    apply TtlvOperation operation <+>
                    optional TtlvUniqueBatchItemID uniqueBatchItemId <+>
                    apply TtlvResultStatus resultStatus <+>
                    apply TtlvResultReason resultReason <+>
                    optional TtlvResultMessage resultMessage <+>
                    apply TtlvAsynchronousCorrelationValue asynchronousCorrelationValue <+>
                    apply TtlvResponsePayload tstruct <+>
                    optional TtlvMessageExtension messageExtension
                    
                    
-- TODO requests/responses                    
