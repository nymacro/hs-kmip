{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Ttlv.Data where

import Ttlv.Tag

import Control.Lens
import Data.Time
import qualified Data.ByteString.Lazy as L

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv { getTtlvTag :: TtlvTag, getTtlvData :: TtlvData }
            deriving (Show, Eq)

-- | Tag data
data TtlvData = TtlvStructure { ttlvStructure :: [Ttlv] }
              | TtlvInt { ttlvInt :: Int }
              | TtlvLongInt { ttlvLongInt :: Integer }
              | TtlvBigInt { ttlvBigInt :: Integer }
              | TtlvEnum { ttlvEnum :: Int }
              | TtlvBool { ttlvBool :: Bool }
              | TtlvString { ttlvString :: String }
              | TtlvByteString { ttlvByteString :: L.ByteString }
              | TtlvDateTime { ttlvDateTime :: UTCTime }
              | TtlvInterval { ttlvInterval :: Int }
              deriving (Show, Eq)

makeLenses ''Ttlv

makePrisms ''TtlvData
makePrisms ''TtlvTag

class TtlvBoxable a where
  box :: a -> TtlvData

class TtlvUnboxable a where
  unbox :: TtlvData -> Maybe a

instance TtlvBoxable Int where
  box = TtlvInt
instance TtlvUnboxable Int where
  unbox (TtlvInt a) = Just a
  unbox _ = Nothing

instance TtlvBoxable String where
  box = TtlvString
instance TtlvUnboxable String where
  unbox (TtlvString a) = Just a
  unbox _ = Nothing

instance TtlvBoxable Integer where
  box = TtlvLongInt
instance TtlvUnboxable Integer where
  unbox (TtlvLongInt a) = Just a
  unbox _ = Nothing

instance TtlvBoxable Bool where
  box = TtlvBool
instance TtlvUnboxable Bool where
  unbox (TtlvBool a) = Just a
  unbox _ = Nothing
