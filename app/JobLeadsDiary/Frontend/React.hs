{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module JobLeadsDiary.Frontend.React (
  staticFiles
 ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.FileEmbed
import qualified Data.Text as T
import Language.Haskell.TH
import System.FilePath
import System.Directory
import System.Process

import JobLeadsDiary.Frontend.Filetypes
import JobLeadsDiary.Frontend.Types

staticFiles :: EmbedTree
staticFiles = $(do
  runIO $ callCommand "cd frontend; npm run build"
  mfp <- runIO $ findExecutable "file"
  fep <- case mfp of
    Nothing -> fail "Could not find \"file\" executable!"
    Just p -> return p
  let
    b :: [([String]->[String],String)] -> Q [Exp]
    b [] = return []
    b ((pa,p):r) = runIO (doesDirectoryExist p) >>= \de -> if de
      then do
        s <- runIO $ listDirectory p
        b (r ++ map (\fn -> ((pa . (fn:)), p </> fn)) s)
      else do
        mt <- case suffixMime p of
          Nothing -> runIO $ readProcess fep ["-b","--mime-type",p] ""
          Just s -> return s
        fc <- embedFile p
        e <- [| oneFile
          $(return $ ListE $ map (LitE . StringL) $ pa [])
          $(return $ LitE $ StringL mt)
          $(return fc)
         |]
        (e:) <$> b r
  cl <- b [(id,"frontend/build")]
  return $ AppE (VarE $ mkName "mconcat") $ ListE cl
 )
