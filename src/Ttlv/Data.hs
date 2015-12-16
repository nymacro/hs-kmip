{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Ttlv.Data ( TtlvData(..)
                 , Ttlv(..)
                 , Boxable(..)
                 , Unboxable(..) ) where

import           Ttlv.Tag

import qualified Data.ByteString as B
import           Data.Time
import           Data.Text

-- | Tag data
data TtlvData = TtlvStructure { ttlvStructure :: ![Ttlv] }
              | TtlvInt { ttlvInt :: !Int }
              | TtlvLongInt { ttlvLongInt :: !Integer }
              | TtlvBigInt { ttlvBigInt :: !Integer }
              | TtlvEnum { ttlvEnum :: !Int }
              | TtlvBool { ttlvBool :: !Bool }
              | TtlvString { ttlvString :: !Text }
              | TtlvByteString { ttlvByteString :: !B.ByteString }
              | TtlvDateTime { ttlvDateTime :: !UTCTime }
              | TtlvInterval { ttlvInterval :: !Int }
              deriving (Show, Eq)

-- | Data type representing Ttlv-encoding of KMIP message
data Ttlv = Ttlv { getTtlvTag :: !Tag, getTtlvData :: !TtlvData }
          deriving (Show, Eq)

class Boxable a where
  box :: a -> TtlvData

class Unboxable a where
  unbox :: TtlvData -> Maybe a

instance Boxable Int where
  box = TtlvInt
instance Unboxable Int where
  unbox (TtlvInt a) = Just a
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
