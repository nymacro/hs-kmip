{-# LANGUAGE TemplateHaskell, FlexibleInstances, DataKinds, DeriveFunctor #-}
module Ttlv.Data where

import Ttlv.Tag

import Control.Lens
import Data.Time
import qualified Data.ByteString.Lazy as L

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

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv { getTtlvTag :: Tag, getTtlvData :: TtlvData }
          deriving (Show, Eq)

makeLenses ''Ttlv
makeLenses ''Tag'

makePrisms ''TtlvData
makePrisms ''TtlvTag

class Boxable a where
  box :: a -> TtlvData

class Unboxable a where
  unbox :: TtlvData -> Maybe a

instance Boxable Int where
  box = TtlvInt
instance Unboxable Int where
  unbox (TtlvInt a) = Just a
  unbox _ = Nothing

instance Boxable String where
  box = TtlvString
instance Unboxable String where
  unbox (TtlvString a) = Just a
  unbox _ = Nothing

instance Boxable Integer where
  box = TtlvLongInt
instance Unboxable Integer where
  unbox (TtlvLongInt a) = Just a
  unbox _ = Nothing

instance Boxable Bool where
  box = TtlvBool
instance Unboxable Bool where
  unbox (TtlvBool a) = Just a
  unbox _ = Nothing
