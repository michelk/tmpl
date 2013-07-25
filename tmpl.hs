{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Encoding as E

import Data.Text.Template


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error usage)
  let cntx = context . splitStr . head  $ args
  tmpl <- TIO.getContents
  S.putStr $ E.encodeUtf8 $ substitute tmpl cntx

usage :: String
usage = "Usage: tmpl var1=uno:var2=dos < myfile.tmpl > myfile.txt"

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

splitStr :: String -> [(T.Text, T.Text)]
splitStr s = map splitEq (T.split (== ':') (T.pack s))
  where
    splitEq s = (k,v)
      where
        k:v:[] = T.split (== '=') s
