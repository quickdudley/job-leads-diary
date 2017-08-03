{-# LANGUAGE OverloadedStrings #-}
module JobLeadsDiary.Ajax (

 ) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Text.JSON

import Network.HTTP.Types.Header
import Network.Wai

import JobLeadsDiary.Database

trim :: T.Text -> T.Text
trim x = let
  (_,l) = T.break (not . isSpace) x
  go t = let
    (a,b) = T.break isSpace t
    (c,d) = T.break (not . isSpace) b
    in if T.null d
      then a
      else mconcat [a, c, go d]
  in go l

crumbs :: T.Text -> [(T.Text,T.Text)]
crumbs r = map (\p -> let
  (k,f) = T.breakOn "=" p
  v = case T.uncons f of
    Nothing -> ""
    Just (_,v') -> v'
  in (trim k, trim v)
 ) $ T.splitOn ";" r

existingLogin :: Request -> DBM (Maybe User)
existingLogin rq = let
  rc = map snd $ filter ((== hCookie) . fst) $ requestHeaders rq
  dc = map (T.decodeUtf8With T.lenientDecode) rc
  cc = concatMap crumbs dc
  lc = find "jldSession" cc
  in case lc of
    Nothing -> return Nothing
    Just uc -> userWithCookie uc
