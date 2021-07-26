module Colors (priorityColor)  where

import Graphics.Image.ColorSpace

-- | Given the priority of status, a color function is returned
priorityColor :: Int -> ((Int, Int) -> Pixel RGB Double)
priorityColor n = case n of
                  1 -> red
                  2 -> orange
                  3 -> yellow
                  4 -> green
                  5 -> blue
                  6 -> purple
                  7 -> pink
                  _ -> black 

-- | Greates a PixelRGB with precision Double, 
-- so a pixel 255 255 255 becomes PixelRGB 1.0 1.0 1.0
pixel :: Int -> Int -> Int -> Pixel RGB Double
pixel r g b = PixelRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) / 255

red ::  (Int, Int) -> Pixel RGB Double
red _ = pixel 255 127 126

orange :: (Int, Int) -> Pixel RGB Double
orange _ = pixel 255 190 126

yellow :: (Int, Int) -> Pixel RGB Double
yellow _ = pixel 255 255 127

green :: (Int, Int) -> Pixel RGB Double
green _ = pixel 126 255 126

blue :: (Int, Int) -> Pixel RGB Double
blue _ = pixel 127 190 255

purple :: (Int, Int) -> Pixel RGB Double
purple _ = pixel 127 126 255

pink :: (Int, Int) -> Pixel RGB Double
pink _ = pixel 255 126 254

black :: (Int, Int) -> Pixel RGB Double
black _ = pixel 27 26 27

