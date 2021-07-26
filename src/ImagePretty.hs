module ImagePretty (toSquare, drawTier) where

import Data.Function
import Prelude as P
import qualified Graphics.Image as I
import qualified Graphics.Image.Interface as Interface
import Colors
import TierList

-- | Scales and crops an image, so it will look like as a square,
-- based on the smalles size of the image
toSquare :: 
    Interface.Array arr cs e
    => Int 
    -> I.Image arr cs e 
    -> I.Image arr cs e
toSquare size image
    | width < height = cropAndScale width
    | width >= height = cropAndScale height
    where width = I.cols image
          height = I.rows image
          crop' = I.crop (0,0) (size, size)
          scale' factors = I.scale I.Bilinear I.Edge factors image
          cropAndScale smallestSize = crop' $ scale' $ scaleFactor size smallestSize


-- | Calculates a scale factor given the spected size and the current size
-- and always is a real positive number
scaleFactor :: Int -> Int -> (Double, Double)
scaleFactor expectedSize 
            currentSize = (factor, factor)
                          where factor = expectedSize `divide` currentSize

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral


-- | Draws the tier image, given the Tier, size of each square 
-- and the total squares per row. The size is needed for drawing the
-- first square at the beginning that indicates the tier status.
drawTier ::    Tier
            -> Int
            -> Int
            -> I.Image I.VU I.RGB Double
drawTier (Tier _ priority images)
         size 
         squaresPerRow = let chunks = map (fillChunk squaresPerRow size) $ avoidEmpty  $ chunksOf squaresPerRow images
                             prioritySquare = square (priorityColor priority) size
                             -- This works as the base case when joining the images topToBottom
                             emptyImage = I.makeImageR I.VU (1, (squaresPerRow + 1) * size) $ priorityColor 0
                         in foldr I.topToBottom emptyImage $ map (foldl I.leftToRight prioritySquare) chunks  
                         

-- | This helps to fill the chuks if chucksOf returns [], because map
-- cannot be applied to just [], it needs to be [[]]
avoidEmpty :: [[a]] -> [[a]]
avoidEmpty [] = [[]]
avoidEmpty xs = xs


-- | Fills the lists with images of the same color, so
-- when joining the images all lists have the same size
fillChunk ::    Int
             -> Int
             -> [I.Image I.VU I.RGB Double]
             -> [I.Image I.VU I.RGB Double]
fillChunk max size images = if length images < max
                       then fillChunk max size $ images ++ [blackSquare]
                       else images
                       where blackSquare = square (priorityColor 0) size


-- | Creates a solid square image, given its size 
-- in pixels and a color function
square :: Interface.ColorSpace cs e 
          => ((Int, Int) -> I.Pixel cs e) 
          -> Int 
          -> I.Image I.VU cs e
square color size = I.makeImageR I.VU (size, size) color


-- Example:  chunksOf 2 [1,2,3,4,5] -> [[1,2],[3,4],[5]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
                where (as, bs) = splitAt n xs
