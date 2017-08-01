module JobLeadsDiary.Frontend.Filetypes (
  suffixMime
 ) where

import Data.List
import qualified Data.Map as M

suffixMime n = foldr (\(s,m) r -> if s `isSuffixOf` n
  then Just m
  else r
 ) Nothing $ M.toList mimeMap

mimeMap :: M.Map String String
mimeMap = M.fromList [
  (".css","text/css"),
  (".html","text/html"),
  (".js","application/javascript"),
  (".json","application/json"),
  (".map","applicataion/json"),
  (".svg","image/svg+xml")
 ]