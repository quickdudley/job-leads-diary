module JobLeadsDiary.Frontend.Types (
  EmbedTree (..)
 ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T

data EmbedTree = EmbedFile T.Text BS.ByteString |
  EmbedTree (M.Map T.Text EmbedTree)

instance Monoid EmbedTree where
  mempty = EmbedTree M.empty
  mappend a@(EmbedFile _ _) _ = a
  mappend _ b@(EmbedFile _ _) = b
  mappend (EmbedTree a) (EmbedTree b) = EmbedTree (M.unionWith mappend a b)
