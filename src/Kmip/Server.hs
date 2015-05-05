{-# LANGUAGE OverloadedStrings #-}
module Kmip.Server where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)

import Data.ByteString.Lazy (fromStrict, toStrict)
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
    case decode ttlv of
     Right x -> case runTtlvParser requestMessage (decodeTtlv $ fromStrict x) of
                 Right _ -> html $ pack "OK!"
                 Left  e -> html $ pack $ mconcat e
     Left _ -> html "bad"
