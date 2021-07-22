module ImagePretty where

import Data.Function
import Prelude as P
import qualified Graphics.Image as I
import qualified Graphics.Image.Interface as Interface


-- | Scales and crops an image, so it will look like as a square
makeSquare :: 
    Interface.Array arr cs e
    => Int 
    -> I.Image arr cs e 
    -> I.Image arr cs e
makeSquare size image
    | width < height = crop' $ scale' ( scaleFactor size width ) image
    | width >= height = crop' $ scale' ( scaleFactor size height ) image
    where width = I.cols image
          height = I.rows image
          crop' = I.crop (0,0) (size, size)
          scale' = I.scale I.Bilinear I.Edge




-- | Calculates a scale factor given the spected size and the current size
-- and always is a real positive number
scaleFactor :: Int -> Int -> (Double, Double)
scaleFactor expectedSize currentSize = (factor, factor)
                                       where factor = expectedSize `divide` currentSize

divide :: Int -> Int -> Double
divide = (/) `on` fromIntegral
