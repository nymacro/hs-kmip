module Ttlv.Client where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Parser
-- import Ttlv.Message
-- import Ttlv.Objects
-- import Ttlv.Structures
import Ttlv.Enum

import qualified Data.ByteString.Lazy as L

data Attribute         = Attribute String TtlvData
type TemplateAttribute = [Attribute]

attributeToTtlv :: Attribute -> Ttlv
attributeToTtlv (Attribute n v) = structure TtlvAttribute [ tag TtlvAttributeName n
                                                          , Ttlv TtlvAttributeValue v ]

attribute :: (TtlvBoxable a) => String -> a -> Ttlv
attribute n v = structure TtlvAttribute [ tag TtlvAttributeName n
                                        , tag TtlvAttributeValue v ]

templateAttributeToTtlv :: TtlvTag -> TemplateAttribute -> Ttlv
templateAttributeToTtlv tag xs = Ttlv tag (TtlvStructure (fmap attributeToTtlv xs))

tag :: (TtlvBoxable a) => TtlvTag -> a -> Ttlv
tag t x = Ttlv t (box x)

int :: TtlvTag -> Int -> Ttlv
int tag = Ttlv tag . TtlvInt

batchCount = int TtlvBatchCount

structure :: TtlvTag -> [Ttlv] -> Ttlv
structure tag = Ttlv tag . TtlvStructure

protocol :: Int -> Int -> Ttlv
protocol maj min = structure TtlvProtocolVersion [ tag TtlvProtocolVersionMajor maj
                                                 , tag TtlvProtocolVersionMinor min]

requestMessage = structure TtlvRequestMessage
requestHeader = structure TtlvRequestHeader
batchItem = structure TtlvBatchItem

enum :: (TtlvEnumType a) => TtlvTag ->  a -> Ttlv
enum t e = Ttlv t (toTtlvEnumType e)

defaultTemplate :: TemplateAttribute
defaultTemplate = []

-- find a better way implement message structure

create :: ObjectType -> TemplateAttribute -> Ttlv
create o t = requestMessage [ requestHeader [ protocol 1 0
                                            , batchCount 1]
                            , batchItem [ enum TtlvOperation Create
                                        , structure TtlvRequestPayload payload
                                          ] ]
  where payload = [ enum TtlvObjectType o
                  , templateAttributeToTtlv TtlvTemplateAttribute t]

createKeyPair :: TemplateAttribute -> TemplateAttribute -> TemplateAttribute -> Ttlv
createKeyPair com priv pub = requestMessage [ requestHeader [ protocol 1 0
                                                            , batchCount 1 ]
                                            , batchItem [ enum TtlvOperation CreateKeyPair
                                                        , structure TtlvRequestPayload payload
                                                        ] ]
  where payload = [ templateAttributeToTtlv TtlvCommonTemplateAttribute com
                  , templateAttributeToTtlv TtlvPrivateKeyTemplateAttribute priv
                  , templateAttributeToTtlv TtlvPublicKeyTemplateAttribute pub ]

-- TODO actual connection context and communication

-- data KmipContext = KmipContext { getHostname :: String
--                                , getPort :: Int
--                                , getSSL :: Bool }
-- 
-- request :: KmipContext -> Ttlv -> Ttlv
-- request ctx r = 
