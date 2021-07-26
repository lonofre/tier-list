module Main where

import qualified Data.ByteString.Lazy as B
import qualified Graphics.Image as I
import Graphics.Image.IO
import Graphics.Image.Interface as Interface
import Prelude as P
import System.Directory
import Data.Aeson as A
import Config
import TierList
import ImagePretty
import Colors

configFile :: FilePath
configFile = "tierlist.json"


main :: IO ()
main = do
       input <- B.readFile configFile
       let json = A.decode input :: Maybe TierConfig
       case json of
            Nothing -> putStrLn "Can't parse the json file "
            Just config -> processConfig config 


processConfig :: TierConfig -> IO ()
processConfig (TierConfig
               size
               squaresPerRow
               directories) = do
                    tiers <- sequence $ P.map (storeTier size) directories
                    let tierImages = P.map (\tier -> drawTier tier size squaresPerRow) tiers
                    let emptyImage = I.makeImageR I.VU (1, (squaresPerRow + 1) * size) $ priorityColor 10 
                    let finalImage = P.foldr I.topToBottom emptyImage tierImages
                    writeImage "Tier-List.jpg" finalImage
                    
                    
-- | Stores the TierDir content into tiers, with the
-- images that will be used
storeTier :: Int -> TierDir -> IO Tier
storeTier size 
          (TierDir name directory priority) = do
                                    path <- imagePaths directory
                                    images <- readImages path
                                    let modifiedImages = P.map (toSquare size) images
                                    return $ Tier name priority modifiedImages


readImages :: [FilePath] 
              -> IO [I.Image I.VU I.RGB Double]
readImages paths = sequence $ P.map (\x -> I.readImageRGB I.VU x) paths


-- | Gets the image paths given its directory, so the
-- result it's the directory path + image filename
imagePaths :: FilePath -> IO [FilePath]
imagePaths path =  do
                   contents <- listDirectory path 
                   let contentsWithPath = P.map (\x -> path ++ "/" ++ x) contents
                   return contentsWithPath
