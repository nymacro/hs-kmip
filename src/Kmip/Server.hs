{-# LANGUAGE OverloadedStrings #-}
module Kmip.Server where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)

import Data.ByteString.Base64

import Ttlv.Validator.Structures (runTtlvParser)
import Ttlv.Validator.Message
import Ttlv.Parser

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    html $ mconcat ["Hello world"]
  post "/validate" $ do
    ttlv <- param "ttlv"
    let validate t = case (runTtlvParser requestMessage (decodeTtlv t)) of
                      Right t -> pack "OK!"
                      Left  e -> pack $ mconcat e
    html $ validate ttlv
