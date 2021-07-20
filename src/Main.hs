module Main where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Config

configFile :: FilePath
configFile = "tierlist.json"

main :: IO ()
main = do
  file <- B.readFile configFile
  let config = decode file :: Maybe TierConfig
  case config of
    Just x -> print x
    Nothing -> putStrLn "Can't read the configuration file"
