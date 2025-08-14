module ReadDat1 (readDat1) where

import Data.Aeson
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Lazy as B

readDat1 :: IO ()
readDat1 = do
  curDir <- getCurrentDirectory
  let
    dataDir = curDir ++ "/data"
  testDat <- B.readFile (dataDir ++ "/testDat1.json")
  print testDat
