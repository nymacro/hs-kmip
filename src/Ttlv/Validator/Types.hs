module Ttlv.Validator.Types ( string
                            , stringEq
                            , tStruct
                            , tEnum
                            , tByteString
                            , tString
                            , tBigInt
                            , tInt
                            , tLong
                            , tInterval
                            , tDateTime
                            , tBool) where

import           Data.Text

import           Ttlv.Data
import           Ttlv.Tag
import           Ttlv.Validator.Structures

-- | on: Ttlv
--   check Ttlv data type
string :: TtlvParser Ttlv
string = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  _ -> Left ["not a string"]

-- | on: Ttlv
--   check Ttlv data type and value
stringEq :: Text -> TtlvParser Ttlv
stringEq s = TtlvParser $ \t -> case getTtlvData t of
  TtlvString x -> if x == s
                  then Right t
                  else Left ["mismatch in expected value"]
  _ -> Left ["not a string"]

-- | on: Ttlv
--   check Ttlv data type
tStruct :: TtlvParser Ttlv
tStruct = TtlvParser $ \t -> case getTtlvData t of
  TtlvStructure _ -> Right t
  _ -> Left ["not a structure"]

-- | on: Ttlv
--   check Ttlv data type
tEnum :: TtlvParser Ttlv
tEnum = TtlvParser $ \t -> case getTtlvData t of
  TtlvEnum _ -> Right t
  _ -> Left ["not an enum"]

tByteString :: TtlvParser Ttlv
tByteString = TtlvParser $ \t -> case getTtlvData t of
  TtlvByteString _ -> Right t
  _ -> Left ["not a byte-string"]

tString :: TtlvParser Ttlv
tString = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  _ -> Left ["not a string"]

tBigInt :: TtlvParser Ttlv
tBigInt = TtlvParser $ \t -> case getTtlvData t of
  TtlvBigInt _ -> Right t
  _ -> Left ["not a big int"]

tInt :: TtlvParser Ttlv
tInt = TtlvParser $ \t -> case getTtlvData t of
  TtlvInt _ -> Right t
  _ -> Left ["not an int"]

tLong :: TtlvParser Ttlv
tLong = TtlvParser $ \t -> case getTtlvData t of
  TtlvLongInt _ -> Right t
  _ -> Left ["not an int"]

tInterval :: TtlvParser Ttlv
tInterval = TtlvParser $ \t -> case getTtlvData t of
  TtlvInterval _ -> Right t
  _ -> Left ["not an int"]

tDateTime :: TtlvParser Ttlv
tDateTime = TtlvParser $ \t -> case getTtlvData t of
  TtlvDateTime _ -> Right t
  _ -> Left ["not an int"]

tBool :: TtlvParser Ttlv
tBool = TtlvParser $ \t -> case getTtlvData t of
  TtlvBool _ -> Right t
  _ -> Left ["not a bool"]

