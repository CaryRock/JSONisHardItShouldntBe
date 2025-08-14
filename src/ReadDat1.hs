{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadDat1 (readDat1) where

import GHC.Generics
import Data.Aeson
import Data.Text
import qualified Data.Aeson.Types as A
import System.Directory (getCurrentDirectory)
import qualified Data.ByteString.Lazy as B

newtype Content = Content
  { contents :: String } deriving (Show, Generic)

readDat1 :: IO ()
readDat1 = do
  curDir <- getCurrentDirectory
  let
    dataDir = curDir ++ "/data"
  testDat <- B.readFile (dataDir ++ "/testDat1.json")
  case eitherDecode testDat :: Either String Value of
    Left  _ -> putStrLn "Decode failed"
    Right x -> print x
  --print testDat
