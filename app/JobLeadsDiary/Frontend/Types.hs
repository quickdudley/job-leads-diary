module JobLeadsDiary.Frontend.Types (
  EmbedTree (..),
  oneFile
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

oneFile :: [T.Text] -> T.Text -> BS.ByteString -> EmbedTree
oneFile [] m c = EmbedFile m c
oneFile (a:r) m c = EmbedTree $ M.singleton a $ oneFile r m c