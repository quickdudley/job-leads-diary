module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import JobLeadsDiary.Database
import JobLeadsDiary.Frontend.Types
import JobLeadsDiary.Frontend.React

myTLSSettings = tlsSettings "selfsigned.crt" "selfsigned.key"
mySettings = setPort 8043 defaultSettings

main :: IO ()
main = runTLS myTLSSettings mySettings undefined
