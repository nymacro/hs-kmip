{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Base64

import qualified Ttlv.Validator.Structures as S
import Ttlv.Validator.Message
import Ttlv.Parser

import Ttlv.Tag as T
import Ttlv.Data

import Control.Lens

import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes

mainTitle = "KMIP Tool"

homePage = H.html $ do
  H.head $ H.title mainTitle
  H.body "Hello World!"

stylesheet = ".tag {" ++
             "  border: 1px solid black;" ++
             "  background-color: #ffffff;" ++
             "  margin: 8 auto;" ++
             "}" ++
             ".data {" ++
             "  background-color: #d0d0d0;" ++
             "  margin: 8 auto;" ++
             "  padding-left: 16px;" ++
             "}"

defaultThings :: H.Html -> H.Html
defaultThings body_ = do
  H.head $ do
    H.title mainTitle
    H.style ! type_ "text/css" $ H.string stylesheet
  H.body body_
    
renderTtlv :: Ttlv -> H.Html
renderTtlv t = H.html $ do
  H.div ! class_ "tag" $ do
    H.string $ show (getTtlvTag t)
    H.div ! class_ "data" $
      case getTtlvData t of
       (TtlvStructure  t) -> mapM_ (renderTtlv) t
       (TtlvInt        t) -> H.string $ show t
       (TtlvLongInt    t) -> H.string $ show t
       (TtlvBigInt     t) -> H.string $ show t
       (TtlvEnum       t) -> H.string $ show t
       (TtlvBool       t) -> H.string $ show t
       (TtlvString     t) -> H.string $ show t
       (TtlvByteString t) -> H.string $ show t
       (TtlvDateTime   t) -> H.string $ show t
       (TtlvInterval   t) -> H.string $ show t

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ renderHtml $ homePage

  get "/hello" $ do
    html $ "Hello world"

  post "/display" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> html . renderHtml . defaultThings $ renderTtlv (decodeTtlv $ fromStrict x)
     Left _  -> html "bad input"

  -- validate request or response
  post "/validate" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser (requestMessage S.<|> responseMessage) (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"

  post "/validate/request" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser requestMessage (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"

  post "/validate/response" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> case S.runTtlvParser responseMessage (decodeTtlv $ fromStrict x) of
                 Right _ -> html "ok"
                 Left  e -> html $ pack $ mconcat (map (++ "\n") e)
     Left _ -> html "bad input"
