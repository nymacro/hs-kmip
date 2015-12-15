module Kmip.Client where

import           Ttlv.Data
import           Ttlv.Parser.Binary
import qualified Ttlv.Tag             as T
-- import           Ttlv.Validator.Message
import           Ttlv.Validator.Objects
import           Ttlv.Validator.Structures hiding (tag)
import           Ttlv.Validator.Types
import           Ttlv.Enum

import qualified Data.ByteString.Lazy as L

data Attribute         = Attribute String TtlvData
type TemplateAttribute = [Attribute]

attributeToTtlv :: Attribute -> Ttlv
attributeToTtlv (Attribute n v) = structure T.Attribute [ tag T.AttributeName n
                                                        , Ttlv (T.Tag T.AttributeValue) v ]

attribute :: (Boxable a) => String -> a -> Ttlv
attribute n v = structure T.Attribute [ tag T.AttributeName n
                                      , tag T.AttributeValue v ]

templateAttributeToTtlv :: T.TtlvTag -> TemplateAttribute -> Ttlv
templateAttributeToTtlv tag xs = Ttlv (T.Tag tag) (TtlvStructure (fmap attributeToTtlv xs))

tag :: (Boxable a) => T.TtlvTag -> a -> Ttlv
tag t x = Ttlv (T.Tag t) (box x)

int :: T.TtlvTag -> Int -> Ttlv
int tag = Ttlv (T.Tag tag) . TtlvInt

batchCount = int T.BatchCount

structure :: T.TtlvTag -> [Ttlv] -> Ttlv
structure tag = Ttlv (T.Tag tag) . TtlvStructure

protocol :: Int -> Int -> Ttlv
protocol maj min = structure T.ProtocolVersion [ tag T.ProtocolVersionMajor maj
                                               , tag T.ProtocolVersionMinor min]

requestMessage = structure T.RequestMessage
requestHeader = structure T.RequestHeader
batchItem = structure T.BatchItem

enum :: (TtlvEnumType a) => T.TtlvTag ->  a -> Ttlv
enum t e = Ttlv (T.Tag t) (toTtlvEnumType e)

defaultTemplate :: TemplateAttribute
defaultTemplate = []

-- find a better way implement message structure

request :: [Ttlv] -> Ttlv
request xs = requestMessage $ [ requestHeader [ protocol 1 0
                                              , batchCount $ length xs
                                              ] ] ++ xs

create :: ObjectType -> TemplateAttribute -> Ttlv
create o t = batchItem [ enum T.Operation Create
                       , structure T.RequestPayload payload]
  where payload = [ enum T.ObjectType o
                  , templateAttributeToTtlv T.TemplateAttribute t]

createKeyPair :: TemplateAttribute -> TemplateAttribute -> TemplateAttribute -> Ttlv
createKeyPair com priv pub = batchItem [ enum T.Operation CreateKeyPair
                                       , structure T.RequestPayload payload
                                       ]
  where payload = [ templateAttributeToTtlv T.CommonTemplateAttribute com
                  , templateAttributeToTtlv T.PrivateKeyTemplateAttribute priv
                  , templateAttributeToTtlv T.PublicKeyTemplateAttribute pub ]

-- TODO actual connection context and communication

-- data KmipContext = KmipContext { getHostname :: String
--                                , getPort :: Int
--                                , getSSL :: Bool }
--
-- request :: KmipContext -> Ttlv -> Ttlv
-- request ctx r =


