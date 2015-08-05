{-# LANGUAGE OverloadedStrings #-}
module Kmip.Server where

import           Network.Wai
import           Web.Scotty

import           Data.Monoid                   (mconcat)
import           Data.Text.Lazy                (pack)

import           Data.ByteString.Base64
import           Data.ByteString.Lazy          (fromStrict, toStrict)

import           Ttlv.Parser
import           Ttlv.Validator.Message
import qualified Ttlv.Validator.Structures     as S

import           Ttlv.Data
import           Ttlv.Tag                      as T

import           Text.Blaze                    (ToMarkup (..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes

import qualified Data.Text                     as T
import           Stitch
import           Stitch.Combinators

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
  ".tagName" ? do
    "display" .= "inline"
    "font-size" .= "16"
    "font-weight" .= "bold"
  ".data" ? do
    "border" .= "1px dotted #606060"
    "background-color" .= "#dddddd"
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
renderTtlv t = H.html $
  H.div ! class_ "tag" $ do
    H.div ! class_ "tagName" $ H.string $ show (getTtlvTag t)
    H.div ! class_ "data" $
      case getTtlvData t of
       (TtlvStructure  t) -> typeString "Structure"   >> mapM_ renderTtlv t
       (TtlvInt        t) -> typeString "Int"         >> H.string (show t)
       (TtlvLongInt    t) -> typeString "Long Int"    >> H.string (show t)
       (TtlvBigInt     t) -> typeString "Big Int"     >> H.string (show t)
       (TtlvEnum       t) -> typeString "Enum"        >> H.string (show t)
       (TtlvBool       t) -> typeString "Bool"        >> H.string (show t)
       (TtlvString     t) -> typeString "String"      >> H.string (show t)
       (TtlvByteString t) -> typeString "Byte String" >> H.string (show t)
       (TtlvDateTime   t) -> typeString "Date Time"   >> H.string (show t)
       (TtlvInterval   t) -> typeString "Interval"    >> H.string (show t)
  where typeString s = H.div ! class_ "type" $ H.string s

server :: ScottyM ()
server = do
  get "/" $
    html $ renderHtml homePage

  post "/display" $ do
    ttlv <- body
    html . renderHtml $ do
      H.h1 "KMIP Display"
      let ttlv'       = decodeTtlv ttlv
          validHtml   = H.div ! class_ "good" $ H.string "Validates"
          invalidHtml = H.div ! class_ "bad"  $ H.string "Failed Validation"
      either (const invalidHtml) (const validHtml) $ S.runTtlvParser (requestMessage S.<|> responseMessage) ttlv'
      defaultThings $ renderTtlv ttlv'

  -- validate request or response
  post "/validate" $ do
    ttlv <- body
    case S.runTtlvParser (requestMessage S.<|> responseMessage) (decodeTtlv ttlv) of
     Right _ -> html "ok"
     Left  e -> html $ pack $ mconcat (fmap (++ "\n") e)

  post "/validate/request" $ do
    ttlv <- body
    case S.runTtlvParser requestMessage (decodeTtlv ttlv) of
     Right _ -> html "ok"
     Left  e -> html $ pack $ mconcat (fmap (++ "\n") e)

  post "/validate/response" $ do
    ttlv <- body
    case S.runTtlvParser responseMessage (decodeTtlv ttlv) of
     Right _ -> html "ok"
     Left  e -> html $ pack $ mconcat (fmap (++ "\n") e)
