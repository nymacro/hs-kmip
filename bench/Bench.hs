module Main where

import           Criterion.Main
import           Data.ByteString
import           Kmip10Data
import           Ttlv.Data
import qualified Ttlv.Parser.Binary    as BP
import qualified Ttlv.Parser.Serialize as SP

main :: IO ()
main = defaultMain [ bgroup "cereal" [ bench "create" $ whnf SP.decodeTtlv kmip_1_0__3_1_1_create_request
                                     , bench "atlist" $ whnf SP.decodeTtlv kmip_1_0__4_1_a_get_attribute_list_response ]
                   , bgroup "binary" [ bench "create" $ whnf BP.decodeTtlv kmip_1_0__3_1_1_create_request
                                     , bench "atlist" $ whnf BP.decodeTtlv kmip_1_0__4_1_a_get_attribute_list_response ] ]
