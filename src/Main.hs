module Main where

import qualified Data.ByteString.Lazy as B
import qualified Graphics.Image as I
import Graphics.Image.IO
import Prelude as P
import System.Directory
import Data.Aeson
import Config
import TierList
import ImagePretty

configFile :: FilePath
configFile = "tierlist.json"

main :: IO ()
main = do
       putStrLn "yeah" 


