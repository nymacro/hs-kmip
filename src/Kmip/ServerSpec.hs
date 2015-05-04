{-# LANGUAGE OverloadedStrings #-}
module Kmip.ServerSpec where

import Test.Hspec

import Network.URI
import Network.TCP as TCP
import Network.HTTP

import Data.ByteString
import Data.ByteString.Base64
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Kmip10Data

validateRequest :: ByteString -> Request ByteString
validateRequest t =
  let uri = case parseURI "http://localhost/validate" of
             Just x -> x
  in Request { rqURI = uri
             , rqMethod = POST
             , rqHeaders = [ mkHeader HdrContentType "text/plain" ]
             , rqBody = encode t }

spec :: Spec
spec = do
  describe "Server" $ do
    let runIt t = do
          conn <- TCP.openStream "localhost" 3000
          response <- sendHTTP conn (validateRequest $ toStrict t)
          getResponseBody response

    describe "1.0" $ do
      it "3.1.1" $ do
        rsp <- runIt kmip_1_0__3_1_1_create_request
        rsp `shouldBe` "OK!"
