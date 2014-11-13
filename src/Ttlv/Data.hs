module Ttlv.Data where

import Ttlv.Tag

import Data.Time
import qualified Data.ByteString.Lazy as L

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv TtlvTag TtlvData
            deriving (Show, Eq)

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
              | TtlvRepeated [TtlvData] -- TODO
              deriving (Show, Eq)
