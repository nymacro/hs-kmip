module Ttlv.Validator.Types ( string
                            , stringEq
                            , tstruct
                            , tenum
                            , tbytestring
                            , tstring
                            , tbigint
                            , tint
                            , tlong
                            , tinterval
                            , tdatetime
                            , tbool) where

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
stringEq :: String -> TtlvParser Ttlv
stringEq s = TtlvParser $ \t -> case getTtlvData t of
  TtlvString x -> if x == s
                  then Right t
                  else Left ["mismatch in expected value"]
  _ -> Left ["not a string"]

-- | on: Ttlv
--   check Ttlv data type
tstruct :: TtlvParser Ttlv
tstruct = TtlvParser $ \t -> case getTtlvData t of
  TtlvStructure _ -> Right t
  _ -> Left ["not a structure"]

-- | on: Ttlv
--   check Ttlv data type
tenum :: TtlvParser Ttlv
tenum = TtlvParser $ \t -> case getTtlvData t of
  TtlvEnum _ -> Right t
  _ -> Left ["not an enum"]

tbytestring :: TtlvParser Ttlv
tbytestring = TtlvParser $ \t -> case getTtlvData t of
  TtlvByteString _ -> Right t
  _ -> Left ["not a byte-string"]

tstring :: TtlvParser Ttlv
tstring = TtlvParser $ \t -> case getTtlvData t of
  TtlvString _ -> Right t
  _ -> Left ["not a string"]

tbigint :: TtlvParser Ttlv
tbigint = TtlvParser $ \t -> case getTtlvData t of
  TtlvBigInt _ -> Right t
  _ -> Left ["not a big int"]

tint :: TtlvParser Ttlv
tint = TtlvParser $ \t -> case getTtlvData t of
  TtlvInt _ -> Right t
  _ -> Left ["not an int"]

tlong :: TtlvParser Ttlv
tlong = TtlvParser $ \t -> case getTtlvData t of
  TtlvLongInt _ -> Right t
  _ -> Left ["not an int"]

tinterval :: TtlvParser Ttlv
tinterval = TtlvParser $ \t -> case getTtlvData t of
  TtlvInterval _ -> Right t
  _ -> Left ["not an int"]

tdatetime :: TtlvParser Ttlv
tdatetime = TtlvParser $ \t -> case getTtlvData t of
  TtlvDateTime _ -> Right t
  _ -> Left ["not an int"]

tbool :: TtlvParser Ttlv
tbool = TtlvParser $ \t -> case getTtlvData t of
  TtlvBool _ -> Right t
  _ -> Left ["not a bool"]

