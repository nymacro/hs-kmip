{-# LANGUAGE OverloadedStrings #-}
module Kmip.ServerSpec where

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.Browser
import           Network.HTTP
import           Network.TCP            as TCP
import           Network.URI

import           Data.ByteString.Base64
import           Data.ByteString.Char8  (pack, unpack)
import           Data.ByteString.Lazy   (fromStrict, toStrict)

import           Data.Maybe             (fromJust)

import           Kmip10Data

import qualified Kmip.Server
import qualified Web.Scotty             as S

import           Debug.Trace            (trace)

spec :: Spec
spec = with (S.scottyApp Kmip.Server.server) $
  describe "Server" $ do
    it "should respond to hello" $
      get "/" `shouldRespondWith` 200

    -- run through all data
    describe "Validate" $
      mapM_ (\x -> it ("should validate " ++ fst x) $
                   post "/validate" (snd x) `shouldRespondWith` "ok" { matchStatus = 200 })
        kmip_1_0__all
