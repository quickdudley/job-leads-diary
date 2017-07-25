{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module JobLeadsDiary.Frontend.React (
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

import JobLeadsDiary.Frontend.Types

staticFiles = $(do
  runIO $ callCommand "cd frontend; npm run build"
  mfp <- runIO $ findExecutable "file"
  fep <- case mfp of
    Nothing -> fail "Could not find \"file\" executable!"
    Just p -> return p
  let
    b [] = return []
    b ((pa,p):r) = doesDirectoryExist >>= \de -> if de
      then do
        s <- listDirectory p
        b (r ++ map (\fn -> (pa . (fn:)), p </> fn) s)
      else do
        mt <- readProces fep ["-b","--mime-type",p]
        fc <- embedFile p
        ((mt,fc):) <$> b r
  cl <- runIO $ b [(id,"frontend/")]
 )
