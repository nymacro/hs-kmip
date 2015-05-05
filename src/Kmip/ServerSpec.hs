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

import Kmip10Data

spec :: Spec
spec = do
  describe "Things" $ do
    it "should do things" $ do
      rsp <- simpleHTTP (getRequest "http://localhost:3000/hello")
      body <- fmap (take 100) (getResponseBody rsp)
      body `shouldBe` "hello"
    it "should browse" $ do
      rsp <- browse $ do
        request $ getRequest "http://localhost:3000/hello"
      (rspBody $ snd rsp) `shouldBe` "hello"
