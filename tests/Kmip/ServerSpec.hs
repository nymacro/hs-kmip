{-# LANGUAGE OverloadedStrings #-}
module Kmip.ServerSpec where

import           Test.Hspec
import           Test.Hspec.Wai

import           Kmip10Data

import qualified Kmip.Server
import qualified Web.Scotty             as S

spec :: Spec
spec = with (S.scottyApp Kmip.Server.server) $
  describe "Server" $ do
    it "should respond to hello" $
      get "/" `shouldRespondWith` 200

    -- run through all data
    describe "Validate" $
      let runTest x = it ("should validate " ++ fst x) $
                        post "/validate" (snd x) `shouldRespondWith` "ok" { matchStatus = 200 }
      in mapM_ runTest kmip_1_0__all
