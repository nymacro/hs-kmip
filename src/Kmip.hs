{-# LANGUAGE OverloadedStrings #-}
module Kmip where

import Ttlv
import Network.TLS
import Network.TLS.Extra

-- this will be stuff that uses Ttlv but implements actual KMIP protocol

-- many :: TtlvTag -> Ttlv -> State Ttlv Bool
-- many tag ttlv = 
