{-# LANGUAGE TemplateHaskell   #-}
module Ttlv.Lens where

import Control.Lens

import Ttlv.Tag as T
import Ttlv.Data as D

makeLenses ''T.Tag'
makePrisms ''T.TtlvTag

makeLenses ''D.Ttlv
makePrisms ''D.TtlvData
