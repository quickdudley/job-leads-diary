{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Unlift

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
main = withDatabase "jobleads.sqlite3" $ do
  uio <- askUnliftIO
  liftIO $ runTLS myTLSSettings mySettings $
    (unliftIO uio .) . (. (liftIO .)) . app

app :: Request -> (Response -> DBM ResponseReceived) -> DBM ResponseReceived
app rq respond = case getFile staticFiles $ pathInfo rq of
  Nothing -> respond $ responseLBS status404 [(hContentType,"text/plain")]
    "File not found. To do: make this message look nicer!"
  Just (mt,c) -> respond $ responseLBS ok200 [(hContentType,T.encodeUtf8 mt)]
    (BL.fromStrict c)
