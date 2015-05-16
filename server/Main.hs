{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Base64

import qualified Ttlv.Validator.Structures as S
import Ttlv.Validator.Message
import Ttlv.Parser

import Ttlv.Tag as T

import Control.Lens

main :: IO ()
main = scotty 3000 $ do
  get "/hello" $ do
    html $ "Hello world"

  -- validate request or response
  post "/validate" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser (requestMessage S.<|> responseMessage) (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"

  post "/validate/request" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser requestMessage (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"

  post "/validate/response" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser responseMessage (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"
