{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Encoding as E
import System.Directory (doesFileExist)

import Data.Text.Template

usage :: String
usage = unlines ["Usage:"
               ,"\ttmpl var1=uno:var2=dos < myfile.tmpl > myfile.txt"
               ,"\ttmpl myfile.tmpl  < vars.conf > myfile.txt"
               ,"\ttmpl myfile.tmpl vars.conf  > myfile.txt"
               ]

main :: IO ()
main = do
  args <- getArgs
  (tmpl,cntx) <-
      case length args of
        1 -> do
          let f = head args
          isFile <- doesFileExist f
          if isFile
             then do
               cntx <- fmap (context . splitStr) TIO.getContents
               tmpl <- TIO.readFile f
               return (tmpl,cntx)
             else do
               let cntx = context . splitStr . T.pack $ f
               tmpl <- TIO.getContents
               return (tmpl,cntx)

        2 -> do
          let tmplF:cntxF:[] = args
          existTmplF <- doesFileExist tmplF
          existCntxF <- doesFileExist cntxF
          when (not  existTmplF || not existCntxF) (fail usage)
          tmpl <- TIO.readFile tmplF
          cntx <- fmap (context . splitStr) $ TIO.readFile cntxF
          return (tmpl,cntx)

        _ -> error usage

  S.putStr $ E.encodeUtf8 $ substitute tmpl cntx


-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

-- | First split Text on ':' or newline; second split on '='
splitStr :: T.Text -> [(T.Text, T.Text)]
splitStr s = map splitEq (T.split (\c -> c `elem` [':','\n'] ) s)
  where
    splitEq s = (k,v)
      where
        k:v:[] = map chomp $ T.split (== '=') s
    chomp = T.replace " " ""
