module Ttlv.ClientSpec where

import Ttlv.Client
import Ttlv.Enum
import Ttlv.Structures (runTtlvParser)
import qualified Ttlv.Objects
import qualified Ttlv.Message as M

import Test.Hspec

spec :: Spec
spec = do
  describe "Client" $ do
    let req ttlv = runTtlvParser M.requestMessage ttlv `shouldBe` Right ttlv
    describe "create" $ do
      it "certificate default request" $ do
        req $ create Certificate []
    describe "create key pair" $ do
      it "key pair default request" $ do
        req $ createKeyPair [] [] []
