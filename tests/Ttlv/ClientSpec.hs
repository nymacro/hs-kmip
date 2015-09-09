module Ttlv.ClientSpec where

import           Kmip.Client
import           Ttlv.Enum
import qualified Ttlv.Validator.Message    as M
import qualified Ttlv.Validator.Objects
import           Ttlv.Validator.Structures (runTtlvParser)

import           Test.Hspec

spec :: Spec
spec = do
  describe "Client" $ do
    let req ttlv = runTtlvParser M.requestMessage (request ttlv) `shouldBe` Right (request ttlv)
    describe "create" $ do
      it "certificate default request" $ do
        req [ create Certificate [] ]
    describe "create key pair" $ do
      it "key pair default request" $ do
        req [ createKeyPair [] [] [] ]
    describe "batch message" $ do
      it "should generate batch" $ do
        req [ create Certificate []
            , createKeyPair [] [] [] ]
