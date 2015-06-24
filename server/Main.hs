module Main where
import Web.Scotty
import qualified Kmip.Server

main :: IO ()
main = scotty 3000 Kmip.Server.server

