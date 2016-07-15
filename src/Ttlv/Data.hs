{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ttlv.Data ( TtlvData(..)
                 , Ttlv(..)
                 , Boxable(..)
                 , Unboxable(..) ) where

import           Ttlv.Tag

import qualified Data.ByteString as B
import           Data.Int
import           Data.Text
import           Data.Time
import           Data.Word

-- | Tag data
data TtlvData = TtlvStructure { ttlvStructure :: ![Ttlv] }
              | TtlvInt { ttlvInt :: !Int32 }
              | TtlvLongInt { ttlvLongInt :: !Int64 }
              | TtlvBigInt { ttlvBigInt :: !Integer }
              | TtlvEnum { ttlvEnum :: !Word32 }
              | TtlvBool { ttlvBool :: !Bool }
              | TtlvString { ttlvString :: !Text }
              | TtlvByteString { ttlvByteString :: !B.ByteString }
              | TtlvDateTime { ttlvDateTime :: !UTCTime }
              | TtlvInterval { ttlvInterval :: !Word32 }
              deriving (Show, Eq)

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv { getTtlvTag :: !Tag, getTtlvData :: !TtlvData }
          deriving (Show, Eq)

class Boxable a where
  box :: a -> TtlvData

class Unboxable a where
  unbox :: TtlvData -> Maybe a

instance Boxable Int32 where
  box = TtlvInt
instance Unboxable Int32 where
  unbox (TtlvInt a) = Just a
  unbox _ = Nothing

instance Boxable Int where
  box = TtlvInt . fromIntegral
instance Unboxable Int where
  unbox (TtlvInt a) = Just $ fromIntegral a
  unbox _ = Nothing

instance Boxable Text where
  box = TtlvString
instance Unboxable Text where
  unbox (TtlvString a) = Just a
  unbox _ = Nothing

instance Boxable String where
  box = TtlvString . pack
instance Unboxable String where
  unbox (TtlvString a) = Just $ unpack a
  unbox _ = Nothing

instance Boxable Int64 where
  box = TtlvLongInt
instance Unboxable Int64 where
  unbox (TtlvLongInt a) = Just a
  unbox _ = Nothing

instance Boxable Bool where
  box = TtlvBool
instance Unboxable Bool where
  unbox (TtlvBool a) = Just a
  unbox _ = Nothing
