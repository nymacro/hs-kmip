module Ttlv.Validator.MessagesSpec where

import           Test.Hspec

import           Control.Applicative       ((<|>))
import           Ttlv.Data
import qualified Ttlv.Enum                 as TEnum
import qualified Ttlv.Tag                  as T
import           Ttlv.Validator.Message
import           Ttlv.Validator.Structures
import Data.Either (isLeft, isRight)

ttlv :: T.TtlvTag -> TtlvData -> Ttlv
ttlv t = Ttlv (T.Tag t)

spec :: Spec
spec = do
  describe "Messages" $ do
    describe "Protocol Version" $ do
      it "1.0" $ do
        let p = ttlv T.ProtocolVersion (TtlvStructure [ttlv T.ProtocolVersionMajor (TtlvInt 1),
                                                       ttlv T.ProtocolVersionMinor (TtlvInt 0)])
        runTtlvParser protocolVersion p `shouldBe` Right p
      it "1.9 (invalid)" $ do
        let p = ttlv T.ProtocolVersion (TtlvStructure [ttlv T.ProtocolVersionMajor (TtlvInt 1),
                                                       ttlv T.ProtocolVersionMinor (TtlvInt 9)])
        runTtlvParser protocolVersion p `shouldSatisfy` isLeft
      it "9.0 (invalid)" $ do
        let p = ttlv T.ProtocolVersion (TtlvStructure [ttlv T.ProtocolVersionMajor (TtlvInt 9),
                                                       ttlv T.ProtocolVersionMinor (TtlvInt 0)])
        runTtlvParser protocolVersion p `shouldSatisfy` isLeft
