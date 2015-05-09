{-# LANGUAGE OverloadedStrings #-}
module Kmip.ServerSpec where

import Test.Hspec

import Network.URI
import Network.TCP as TCP
import Network.HTTP
import Network.Browser

import Data.ByteString.Base64
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Maybe (fromJust)

import Kmip10Data

-- Network.HTTP doesn't like "localhost"
validate ttlv = simpleHTTP $ req ttlv
  where req t = Request { rqURI = fromJust $ parseURI "http://127.0.0.1:3000/validate"
                        , rqMethod = POST
                        , rqHeaders = [ mkHeader HdrContentType "application/x-www-form-urlencoded"
                                      , mkHeader HdrContentLength (show $ length t)
                                      ]
                        , rqBody = t }

spec :: Spec
spec = do
  describe "Server" $ do
    it "should respond to hello" $ do
      rsp <- simpleHTTP (getRequest "http://127.0.0.1:3000/hello")
      body <- fmap (take 100) (getResponseBody rsp)
      body `shouldBe` "Hello world"

    -- run through all data
    describe "Validate" $ do
      mapM_ (\x -> it ("should validate " ++ fst x) $ do
                rsp <- validate $ unpack $ encode $ toStrict $ snd x
                body <- fmap (take 100) (getResponseBody rsp)
                body `shouldBe` "ok") kmip_1_0__all
