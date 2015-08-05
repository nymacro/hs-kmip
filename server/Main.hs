module Main where
import qualified Kmip.Server
import           Web.Scotty

main :: IO ()
main = scotty 3000 Kmip.Server.server
