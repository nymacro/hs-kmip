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

-- import Control.Lens

import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes

import Stitch
import Stitch.Combinators
import qualified Data.Text as T

mainTitle = "KMIP Tool"

homePage = H.html $ do
  H.head $ H.title mainTitle
  H.body "Hello World!"

stylesheet = renderCSS $ do
  "body" ? do
    "background-color" .= "#fffff0"
    "color" .= "#303030"
  ".tag" ? do
    "border" .= "1px solid #303030"
    "background-color" .= "#cccccc"
    "margin" .= "8 auto"
    "padding" .= "4px"
  ".tagName" ? do
    "display" .= "inline"
    "font-size" .= "16"
    "font-weight" .= "bold"
    "padding" .= "4px"
  ".data" ? do
    "border" .= "1px dotted #606060"
    "background-color" .= "#dddddd"
    "margin" .= "8 auto"
    "padding" .= "4px"
  ".type" ? do
    "display" .= "inline"
    "font-style" .= "italic"
    "border" .= "1px solid #a0a0a0"
    "background-color" .= "#f0f0f0"
    "margin-right" .= "16px"
    "padding" .= "4px"
  ".good" ? do
    "display" .= "inline"
    "background-color" .= "#60ff60"
    "color" .= "#103010"
    "border" .= "2px solid #308830"
    "padding" .= "4px"
    "font-size" .= "24"
  ".bad" ? do
    "display" .= "inline"
    "background-color" .= "#ff6060"
    "color" .= "#301010"
    "border" .= "2px solid #883030"
    "padding" .= "4px"
    "font-size" .= "24"

defaultThings :: H.Html -> H.Html
defaultThings body_ = do
  H.head $ do
    H.title mainTitle
    H.style ! type_ "text/css" $ H.text stylesheet
  H.body body_
    
renderTtlv :: Ttlv -> H.Html
renderTtlv t = H.html $ do
  H.div ! class_ "tag" $ do
    H.div ! class_ "tagName" $ H.string $ show (getTtlvTag t)
    H.div ! class_ "data" $
      case getTtlvData t of
       (TtlvStructure  t) -> typeString "Structure"   >> mapM_ (renderTtlv) t
       (TtlvInt        t) -> typeString "Int"         >> (H.string $ show t)
       (TtlvLongInt    t) -> typeString "Long Int"    >> (H.string $ show t)
       (TtlvBigInt     t) -> typeString "Big Int"     >> (H.string $ show t)
       (TtlvEnum       t) -> typeString "Enum"        >> (H.string $ show t)
       (TtlvBool       t) -> typeString "Bool"        >> (H.string $ show t)
       (TtlvString     t) -> typeString "String"      >> (H.string $ show t)
       (TtlvByteString t) -> typeString "Byte String" >> (H.string $ show t)
       (TtlvDateTime   t) -> typeString "Date Time"   >> (H.string $ show t)
       (TtlvInterval   t) -> typeString "Interval"    >> (H.string $ show t)
  where typeString s = H.div ! class_ "type" $ H.string s

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ renderHtml $ homePage

  get "/hello" $ do
    html $ "Hello world"

  post "/display" $ do
    ttlv <- body
    case decode $ toStrict ttlv of
     Right x -> html . renderHtml $ do
       H.h1 "KMIP Display"
       let ttlv'       = decodeTtlv $ fromStrict x
           validHtml   = H.div ! class_ "good" $ H.string "Validates"
           invalidHtml = H.div ! class_ "bad"  $ H.string "Failed Validation"
       either (\_ -> invalidHtml) (\_ -> validHtml) $ S.runTtlvParser (requestMessage S.<|> responseMessage) ttlv'
       defaultThings $ renderTtlv ttlv'
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
