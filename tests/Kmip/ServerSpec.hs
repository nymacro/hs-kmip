{-# LANGUAGE OverloadedStrings #-}
module Kmip.ServerSpec where

import Test.Hspec
import Test.Hspec.Wai

import Network.URI
import Network.TCP as TCP
import Network.HTTP
import Network.Browser

import Data.ByteString.Base64
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Maybe (fromJust)

import Kmip10Data

import qualified Web.Scotty as S
import qualified Kmip.Server

import Debug.Trace (trace)

spec :: Spec
spec = with (S.scottyApp Kmip.Server.server) $ do
  describe "Server" $ do
    it "should respond to hello" $ do
      get "/hello" `shouldRespondWith` "Hello world" { matchStatus = 200 }

    -- run through all data
    describe "Validate" $ do
      mapM_ (\x -> it ("should validate " ++ fst x) $
                   -- postHtmlForm "/validate" [("ttlv", unpack . encode . toStrict $ snd x)] `shouldRespondWith` "ok" { matchStatus = 200 })
                   post "/validate" (fromStrict . encode . toStrict $ snd x) `shouldRespondWith` "ok" { matchStatus = 200 })
        kmip_1_0__all
