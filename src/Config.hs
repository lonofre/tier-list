{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Aeson

-- This serves to configurate the output image
-- squareSize is the size of a side of an image that will
--      be in the tier list
-- tierRows is the total rows in the tier up to 7
data TierConfig = 
    TierConfig { squareSize :: !Int
                , squaresPerRow :: !Int
                , tierRows :: !Int
                , directories :: TierDirs
    } deriving Show

instance FromJSON TierConfig  where
    parseJSON (Object v) = TierConfig
                           <$> v .: "squareSize"
                           <*> v .: "squaresPerRow"
                           <*> v .: "tierRows"
                           <*> v .: "directories"
    parseJSON _ = mempty


-- The directory paths where the images
-- are and the image configuration
data TierDirs =
    TierDirs { sDir :: String
               , aDir :: String
               , bDir :: String
               , cDir :: String
               , dDir :: String
               , eDir :: String
               , fDir :: String
    } deriving Show 

instance FromJSON TierDirs where
    parseJSON (Object v) = TierDirs
                           <$> v .: "sDir"
                           <*> v .: "aDir"
                           <*> v .: "bDir"
                           <*> v .: "cDir"
                           <*> v .: "dDir"
                           <*> v .: "eDir"
                           <*> v .: "fDir"
    parseJSON _ = mempty
