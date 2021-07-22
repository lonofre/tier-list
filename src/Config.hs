{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Aeson

-- | This serves to configurate the output image
-- squareSize is the size of a side of an image that will
--      be in the tier list
data TierConfig = 
    TierConfig { squareSize :: !Int
                , squaresPerRow :: !Int
                , directories :: [TierDir]
    } deriving Show

instance FromJSON TierConfig  where
    parseJSON (Object v) = TierConfig
                           <$> v .: "squareSize"
                           <*> v .: "squaresPerRow"
                           <*> v .: "directories"
    parseJSON _ = mempty


-- | Contains information about the directory,
-- prefered color for a tier, priority (being 1 the first)
-- and the name of the tier
data TierDir =
    TierDir { name      :: !String
             , directory :: !String
             , priority  :: !Int
             , color :: !String
    } deriving Show 

instance FromJSON TierDir where
    parseJSON (Object v) = TierDir
                           <$> v .: "name"
                           <*> v .: "directory"
                           <*> v .: "priority"
                           <*> v .: "color"
    parseJSON _ = mempty
