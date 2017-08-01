{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import JobLeadsDiary.Database
import JobLeadsDiary.Frontend.Types
import JobLeadsDiary.Frontend.React

myTLSSettings = tlsSettings "selfsigned.crt" "selfsigned.key"
mySettings = setPort 8043 defaultSettings

main :: IO ()
main = runTLS myTLSSettings mySettings app

app :: Application
app rq respond = case getFile staticFiles $ pathInfo rq of
  Nothing -> respond $ responseLBS status404 [(hContentType,"text/plain")]
    "File not found. To do: make this message look nicer!"
  Just (mt,c) -> respond $ responseLBS ok200 [(hContentType,T.encodeUtf8 mt)]
    (BL.fromStrict c)
