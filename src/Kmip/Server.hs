{-# LANGUAGE OverloadedStrings #-}
module Kmip.Server where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Base64

import Ttlv.Validator.Structures (runTtlvParser, (<|>))
import Ttlv.Validator.Message
import Ttlv.Parser

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    html $ "Hello world"

  -- validate request or response
  post "/validate" $ do
    ttlv <- body -- param "ttlv"
    case decode $ toStrict ttlv of
     Right x -> case runTtlvParser (requestMessage <|> responseMessage) (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"
